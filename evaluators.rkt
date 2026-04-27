#lang racket
; This contains all expression evaluation functions (i.e., M_value)
(require "state.rkt")
(require "helpers.rkt")
(require "operators.rkt")
(provide (all-defined-out))


; Takes in an initial state, a operational node, and the throw and return continuations
; Evaluates the node's arguments and updates the state with side effects,
; then if nothing was thrown, applies the operator to the evaluated arguments
; and returns the value and updated state
(define processOperation
  (lambda (state node throw return)
    (processArgs null state (argList node) throw
                 (lambda (evalArgs evalState)
                   (return ((convertOperator (operator node)) evalArgs) evalState)
                   )
                 )
    )
  )


; Takes in a list of evaluated arguments (typically null on first call), an initial state,
; unprocessed arguments (formal parameters), and the throw and return continuations.
; If nothing is thrown, returns the actual parameters and the updated with any side-effects state
(define processArgs
  (lambda (evald state args throw return)
    (if (null? args)
        (return evald state)
        (evaluateExpression (car args) state throw
                            (lambda (val retState)
                              (processArgs (append evald (list val))
                                           retState
                                           (cdr args)
                                           throw
                                           return)
                              )
                            )
        )
    )
  )


; Returns the appropriate evaluation of an expression (variable, literal, or nested expression)
; Input could be any expression
; To allow for side-effects, will also return a state as the 2nd element in a list
(define evaluateExpression
  (lambda (node state throw return)
     (cond
       ; ==========
       ; Is the expression a simple one? I.e. a number, boolean literal, or variable name

       ; If the node is a number, simple return
       [(number? node) (return node state)]

       ; If the node is a symbol, check if it's a boolean literal. If so, return the literal
       [(symbol? node) (if (isBool? node)
                           (return node state)
                           
                           ; If it isn't a boolean literal, it must be a variable
                           ; Return the variable's binding
                           (return (lookupBinding node state) state))]

       ; Ok, the expression is nested (not a literal or variable). Is it numerical?
       [(numerical? (operator node)) (evaluateNum node state throw return)]

       ; Is it a function call?
       [(eq? 'funcall (operator node)) (callFunction (primary (argList node)) (cdr (argList node)) state throw return)]

       ; Must be a condition
      [else (evaluateCondition node state throw return)]
      )
    )
  )




; Takes a condition and a state
; Returns a boolean (TRUE or FALSE)
(define evaluateCondition
  (lambda (node state throw return)
    (cond
      ; Is it a bool literal? Then return it
      [(isBool? node) (return node state)]
      
      ; Is it a variable name? Return it's binding
      [(symbol? node) (return (lookupBinding node state) state)]
      
      ; It must be an operation
      [else (processOperation state node throw return)]
      )
 )
)

; Takes an integer value (can be an expression or a variable name) and a state
; Returns an integer
(define evaluateNum
  (lambda (node state throw return)
    (cond
      ; Is it a number? Return it
      [(number? node) (return node state)]
      
      ; Is it a variable name? Return its binding
      [(symbol? node) (return (lookupBinding node state) state)]
      
      ; It must be an operation, apply the operation and return the value
      [else (processOperation state node throw return)]
      )
    )
  )

; =============
; FUNCTIONS
; =============

; Returns the state with the found function, or errors
(define findFunction
  (lambda (name statementList state)
    (cond
      [(null? statementList) (error "Function called without declaration")]
      ; If the statement is a function declaration with the correct name, yay!
      ; The repeated callings of abstractions is kinda yucky, but defines aren't
      ; allowed in the middle of the cond and it's more cumbersome to define
      ; everything to allow for null statements
      [(and
        (eq? (operator (currentStatement statementList)) 'function)
        (eq? (primary (argList (currentStatement statementList))) name)
        ) (funcDeclare (argList (currentStatement statementList)) state)]
      ; Otherwise, press on
      [else (findFunction name (remainingStatements statementList) state)]
      )
    )
  )


; (M_value, M_state) of a function call.
; When returning, provides a list consisting of the value, and the environment at the time
; When throwing, provides the updated environment at throw time
(define callFunction
  (lambda (name actualParameters state throw return)
    (define closure (lookupBinding name state))
    (define callingLevel (length state))

    ; Call the statementList evaluator with the environment on the body
    ; Note: return will provide a list where the 1st element is the returned value,
    ; and the 2nd is the environment at the time
    (processArgs null state actualParameters throw (lambda (evalArgs evalState)
                                                            (nextState (createEnvironment evalArgs closure state) (getBody closure)
                                                                       ; Next
                                                                       echo
                                                                       ; Break
                                                                       (lambda (brokenState) (error "Break outside of a loop"))
                                                                       ; Continue
                                                                       (lambda (continuedState) (error "Continue outsie of a loop"))
                                                                       ; Return
                                                                       (lambda (value returnedState) (return value (updateHeritage state returnedState)))
                                                                       ; Throw
                                                                       (lambda (exception thrownState) (throw exception (updateHeritage state thrownState)))
                                                                       )
                                                     )
                 )
    )
  )

; ========================
; STATEMENT LIST EVALUATOR
; ========================

; Takes a statementList, line of code to be executed, and other continuations
; Continues until it hits a return
(define nextState
  (lambda (state statementList next break continue return throw)
    
    (define statement (if (null? statementList) null (currentStatement statementList)))
    (define op (operator statement))
    (define args (argList statement))
    (define newState (next state))
    (define testTail (remainingStatements statementList))
    ; Default return
    (define tail (if (null? testTail) (list '(return 0)) testTail))
    
    (cond
      ; If we're defining a function, do that
      [(eq? op 'function) (nextState (funcDeclare args newState) tail echo break continue return throw)]

      ; If we're calling a function without an assignment, update the state accordingly
      ; If the function hasn't been declared yet, we must be inside another function.
      ; Thus, since we need to save our place for proper state machining, but we also need to know the function
      ; to update the state properly, we skip ahead to find the function before declaring it, then proceeding.
      
      [(eq? op 'funcall) ((lambda (funcState)
                            (callFunction (primary args) (cdr args) funcState throw (lambda (value retState)
                                                                                      (nextState retState tail echo break continue return throw)
                                                                                      ))
                            )
                          (if (isDeclared? (primary args) newState)
                             newState
                             (findFunction (primary args) tail newState)
                             ))]
      
      ; If we're throwing, do that after tossing the try block scope
      [(eq? op 'throw) (evaluateExpression (primary args) newState
                                           echoDouble
                                           (lambda (exception thrownState)
                                             (throw exception (tossActiveLayer thrownState))
                                             )
                               )]
      
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
      [(eq? 'return op) (evaluateExpression (primary (argList statement)) newState throw
                                            (lambda (value retState)
                                              (return value (tossActiveLayer retState))
                                              )
                                            )
                        ]
      
      ; If we're assigning or declaring, pass that into next
      [(eq? 'var op) (if (isLive? (primary (argList statement)) newState)
                         (error "Variable already live")
                         (if (secondary? args)
                             (evaluateExpression (secondary args) newState throw
                                                 (lambda (val retState)
                                                   (define nState (declareAssign (primary args) val retState))
                                                   (nextState nState tail echo break continue return throw)
                                                   )
                                                 )
                             (nextState (declare (primary args) newState) tail echo break continue return throw)
                             )
       )]
      [(eq? '= op) (if (isDeclared? (primary (argList statement)) newState)
                       (evaluateExpression (secondary args) newState throw (lambda (val retState)
                                                                             (define nState (assign (primary args) val retState))
                                                                             (nextState nState tail echo break continue return throw)
                                                                             ))
                       (error "Variable undeclared")
                       )]

      ; If it's an if statement, evaluate the condition and apply next appropriately
      [(eq? op 'if) (if (eq? (evaluateExpression (primary args) newState throw (lambda (val retState) val)) TRUE)
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
      [(eq? op 'while) (if (eq? (evaluateExpression (primary args) newState throw (lambda (val retState) val)) TRUE)
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

      ; Helps in debugging
      [else (error "Unrecognized operator when progressing")]
      )
    )
  )