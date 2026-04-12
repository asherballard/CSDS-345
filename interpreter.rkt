#lang racket
(require "helpers.rkt")
(require "state.rkt")
(require "operators.rkt")
(require "evaluators.rkt")
(require "simpleParser.rkt")
(provide (all-defined-out))

; ============================
; INTERPRETER HELPER FUNCTIONS
; ============================
; Functions only related to the interpreter that do intermediate steps

; Helper function that turns a statement into a state mapping for getting "next"
; I pass throw into here because it involved less line changes
(define getStateMapping
  (lambda (statement state)
    (define args (argList statement))
    (define op (operator statement))
    (cond
      ; If the statement is a declaration, check if its a redeclaration, then parse as appropriate
      ; In the case of scope declaration, we want a duplicate name
      [(eq? op 'var) (if (secondary? args)
                             ; If a value was given and it's not live in the scope,
                             ; declare the name in the scope and assign it
                             (lambda (inputState) (assign (primary args) (evaluateExpression (secondary args) inputState) (declare (primary args) inputState)))
                             ; Otherwise, just make a duplicate undeclared binding
                             (lambda (inputState) (declare (primary args) inputState))
                             )]
      ; If the statement is an assignment, check to make sure var is declared
      [(eq? op '=) (lambda (inputState) (assign (primary args) (evaluateExpression (secondary args) inputState) inputState))]

      [(eq? op 'function) (lambda (inputState) (assign (primary args) (cons (secondary args) (ternary args)) (declare (primary args) inputState)))]
      )
    )
  )




; =======================
; STATE MAPPING FUNCTIONS
; =======================
; Each of these functions take in constructs and the current state
; And returns a new state
; I.e. M_state mappings

; Takes a statementList, line of code to be executed, and other continuations
; Continues until it hits a return
(define nextState
  (lambda (state statementList next break continue return throw)
    
    (define statement (if (null? statementList) null (currentStatement statementList)))
    (define op (operator statement))
    (define args (argList statement))
    (define newState (next state))
    (define tail (remainingStatements statementList))
    
    (cond
      ; If we're throwing, do that after tossing the try block scope
      [(eq? op 'throw) (throw (evaluateExpression (primary args) newState) newState)]
      
      ; Check for try-catch start
      [(eq? op 'try)
       ; Isolate type of try-catch block
       (cond
         ; Three args must be a try-catch-finally
         ; Add try and finally to the statementList
         ; If we throw, replace the state and statement list
         [(not (null? (ternary args))) (nextState (initializeNewLayer voidLayer newState) (append (makePairedList (makeBlock (primary args)) (makeBlock (primaryArg (ternary args)))) tail)
                                     echo
                                     break
                                     continue
                                     return
                                     (lambda (exception thrownState)
                                       ; We declare and assign "e" with the exception value in a new scope in the original state (before try)
                                       (nextState (declareAssign (operator (primaryArg (secondary args))) exception (initializeNewLayer voidLayer (tossActiveLayer thrownState)))
                                                  (append
                                                   ; The catch block
                                                   (secondaryArg (secondary args))

                                                   ; Signal to drop the "catch" scope
                                                   '((end))
                                                   
                                                   ; The finally block, treated as such
                                                   (list (makeBlock (primaryArg (ternary args))))
                                                   
                                                   ; The tail
                                                   tail)
                                                  echo
                                                  break
                                                  continue
                                                  return
                                                  throw
                                                  )))]

         ; If 2nd arg is catch and no 3rd arg, we have no finally
         [(eq? (operator (secondary args)) 'catch)
          (nextState newState (addStatement (makeBlock (primary args)) tail)
                     echo
                     break
                     continue
                     return
                     (lambda (exception thrownState)
                       (nextState (declareAssign (operator (primaryArg (secondary args))) exception (initializeNewLayer voidLayer (tossActiveLayer thrownState)))
                                  (append
                                   ; The catch block
                                   (secondaryArg (secondary args))

                                   ; Signal to drop the scope
                                   '((end))

                                   ; the tail
                                   tail)
                                  echo
                                  break
                                  continue
                                  return
                                  throw
                                  )))]

         ; Must be a try-finally
         [else  (nextState newState (addStatement (makeBlock (primary args)) tail)
                     echo
                     break
                     continue
                     return
                     (lambda (exception thrownState)
                       (nextState (tossActiveLayer thrownState)
                                  (addStatement
                                   ; The finally block
                                   (makeBlock (primaryArg (secondary args)))

                                   ; The tail
                                   tail)
                                  echo
                                  break
                                  continue
                                  return
                                  throw
                                  )))]
           )
       ]
      
      ; Check for end of block
      [(eq? op 'end) (nextState (tossActiveLayer newState) tail echo break continue return throw)]

      ; Check for beginning of block
      [(eq? op 'begin) (nextState (initializeNewLayer voidLayer newState)
                                  ; Add the block to the execution stack, with a marker (end) to indicate when to toss the scope
                                  ; There's probably a more elegant way to do this
                                  (append args (addStatement '(end) tail))

                                  ; Not a state-mapping line, reset next
                                  echo

                                  ; When breaking or continuing, we have to toss the block scope
                                  (lambda (brokenState) (break (tossActiveLayer brokenState)))
                                  (lambda (continuedState) (continue (tossActiveLayer continuedState)))

                                  ; Return *should* toss the block scope too, but that doesn't matter yet
                                  return

                                  throw)]
      
      ; If we're returning, do that
      [(eq? 'return op) (return (evaluateExpression (primary (argList statement)) newState) newState)]
      
      ; If we're assigning or declaring, pass that into next
      [(eq? 'var op) (if (isLive? (primary (argList statement)) newState)
                         (error "Variable already live")
                         (nextState newState tail (getStateMapping statement newState) break continue return throw)
       )]
      [(eq? '= op) (if (isDeclared? (primary (argList statement)) newState)
                       (nextState newState tail (getStateMapping statement newState) break continue return throw)
                       (error "Variable undeclared")
                       )]

      ; If it's an if statement, evaluate the condition and apply next appropriately
      [(eq? op 'if) (if (eq? (evaluateCondition (primary args) newState) TRUE)
                        ; True condition means we put the first statement on the statementList
                        (nextState newState (addStatement (secondary args) tail) echo break continue return throw)

                        ; Check for an else condition, put it on the list if exists
                        (if (ternary? args)
                            (nextState newState (addStatement (ternary args) tail) echo break continue return throw)
                            (nextState newState tail echo break continue return throw)
                            )
                        )]

      ; If we break or continue, jump out with the newState
      [(eq? op 'break) (break newState)]
      [(eq? op 'continue) (continue newState)]

      ; If it's a while statement, keep reprocessing the statement until the condition is false or we break
      [(eq? op 'while) (if (eq? (evaluateCondition (primary args) newState) TRUE)
                           ; Put the true statement in front of while so it executes before checking again
                           (nextState newState (addStatement (secondary args) statementList)
                                      ; The while statement itself doesn't affect the state
                                      echo

                                      ; When we break, we take the state at that point and move to the tail
                                      ; Nothing happens with next
                                      (lambda (brokenState) (nextState brokenState tail echo break continue return throw))

                                      ; When we continue, we do the loop again immediately with the updatedState
                                      (lambda (continuedState) (nextState continuedState statementList echo break continue return throw))

                                      ; Don't need to be updated here
                                      return
                                      throw)

                           ; If condition is false, do nothing
                           (nextState newState tail echo break continue return throw)
                           )]

      ;add if for main
      [(eq? op 'function) (nextState newState tail (getStateMapping statement newState) break continue return throw)]

      ;lookup function, add parameters to state, add function body to statement list
      [(eq? op 'funcall) (nextState (assignParams (primary (lookUpBinding (primary args) state))) (cons (secondary (lookUpBinding (primary args) state)) tail) echo break continue return throw)]

      ; Helps in debugging
      [else (error "Unrecognized operator when progressing")]
      )
    )
  )

; ==================
; MAIN FUNCTION
; ==================

; The main interpret function
; Literally just starts the stateProgress lawnmower with an initial state and statementList
; Takes the filename as input, and gives the return value as outputd
(define interpret
  (lambda (filename)
    (nextState voidState (parser filename)
               ; Next
               echo
               ; Break
               (lambda (brokenState) (error "Break outside of a loop"))
               ; Continue
               (lambda (continuedState) (error "Continue outsie of a loop"))
               ; Return
               (lambda (value state) (echo value))
               ; Throw
               (lambda (exception thrownState) (error "Threw outside of a try-catch"))
               )
    )
  )
