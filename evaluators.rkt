#lang racket
; This contains all expression evaluation functions (i.e., M_value)
(require "state.rkt")
(require "helpers.rkt")
(require "operators.rkt")
(provide (all-defined-out))

;
(define getReturnValue
  (lambda (valState)
    (primary valState)
    )
  )

(define getReturnState
  (lambda (valState)
    (secondary valState)
    )
  )

; Helper function that turns a statement into a state mapping for getting "next"
; I pass throw into here because it involved less line changes
(define getStateMapping
  (lambda (statement state)
    (define args (argList statement))
    (define op (operator statement))
    (cond
      [(eq? op 'funcall) (lambda (inputState)
                           (getReturnState (callFunction (primary args) (cdr args) inputState)))]
      
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
      )
    )
  )

; Returns the appropriate evaluation of an expression (variable, literal, or nested expression)
; Input could be any expression
; To allow for side-effects, will also return a state as the 2nd element in a list
(define evaluateExpression
  (lambda (node state)
     (cond
       ; ==========
       ; Is the expression a simple one? I.e. a number, boolean literal, or variable name

       ; If the node is a number, simple return
       [(number? node) (list node state)]

       ; If the node is a symbol, check if it's a boolean literal. If so, return the literal
       [(symbol? node) (if (isBool? node)
                           (list node state)
                           
                           ; If it isn't a boolean literal, it must be a variable
                           ; Return the variable's binding
                           (list (lookupBinding node state) state))]

       ; Ok, the expression is nested (not a literal or variable). Is it numerical?
       [(numerical? (operator node)) (evaluateNum node state)]

       ; Is it a function call?
       [(eq? 'funcall (operator node)) (callFunction (primary (argList node)) (cdr (argList node)) state)]

       ; Must be a condition
      [else (evaluateCondition node state)]
      )
    )
  )

; Side-effect catcher for evaluating expressions
(define evaluateEach-cps
  (lambda (f lis state return)
    (cond
      [(null? lis) (return (list null state))]
      [else (evaluateEach-cps f (cdr lis)
                          (getReturnState (f (car lis) state))
                          (lambda (ret) (cons (getReturnValue (f (car lis) state)) ret))
                          )
            ]
      )
    )
  )


; Takes a condition and a state
; Returns a boolean (TRUE or FALSE)
(define evaluateCondition
  (lambda (node state)
    (cond
      ; Is it a bool literal? Then return it
      [(isBool? node) (list node state)]
      
      ; Is it a variable name? Return it's binding
      [(symbol? node) (list (lookupBinding node state) state)]
      
      ; It must be an operation
      [else (cond
              
             ; If magnitude-based, evaluate each arg as an integer, and return its value
             [(magnitudeBased? (operator node))
                  (list
                   (applyToEach-cps (lambda (x) (evaluateNum x state)) (argList node) (convertOperator (operator node)))
                   
                   )]

             ; Catch not, which only has one argument
             [(eq? '! (operator node))
              ((convertOperator (operator node)) (evaluateCondition (primary (argList node)) state))]
             
             ; If boolean-based, evaluate each arg as such, and return its value
             [(booleanBased? (operator node))
                  ((convertOperator (operator node)) (applyToEach-cps (lambda (x) (evaluateCondition x state)) (argList node) echo))]
                  
             ; If we reach this point, it must be a comparison operator (== or !=).
             ; Thus we call the generic expression evaluator, since these operators can be either numerical or a condition
             [else ((convertOperator (operator node)) (applyToEach-cps (lambda (x) (evaluateExpression x state)) (argList node) echo))]
             )
       ]
      )
 )
)

; Takes an integer value (can be an expression or a variable name) and a state
; Returns an integer
(define evaluateNum
  (lambda (node state)
    (cond
      ; Is it a number? Return it
      [(number? node) node]
      
      ; Is it a variable name? Return its binding
      [(symbol? node) (lookupBinding node state)]
      
      ; It must be an operation, apply the operation and return the value
      [else ((convertOperator (operator node)) (applyToEach-cps (lambda (x) (evaluateNum x state)) (argList node) echo))]
      )
    )
  )

; =============
; FUNCTIONS
; =============


; To shorten declaring functions
(define funcDeclare
  (lambda (args state)
    (define name (primary args))
    (define formalParams (secondary args))
    (define body (ternary args))
      
    (declareAssign name (createClosure formalParams body (length state)) state)
    )
  )

; M_value of a function call. Note: DOES NOT HANDLE FUNCTIONS THAT DON'T RETURN
; When returning, provides a list consisting of the value, and the environment at the time
; When throwing, provides the updated environment at throw time
(define callFunction
  (lambda (name actualParameters state)
    (define closure (lookupBinding name state))

    ; Evaluate the actual parameters using call-by-value
    (define evaluatedParameters
      (applyToEach-cps
       (lambda (node) (evaluateExpression node state))
       actualParameters
       echo))
    
    (define environment (createEnvironment evaluatedParameters closure state))
    (define callingLevel (length state))

    ; Call the statementList evaluator with the environment on the body
    ; Note: return will provide a list where the 1st element is the returned value,
    ; and the 2nd is the environment at the time
    (nextState environment (getBody closure)
               ; Next
               echo
               ; Break
               (lambda (brokenState) (error "Break outside of a loop"))
               ; Continue
               (lambda (continuedState) (error "Continue outsie of a loop"))
               ; Return
               (lambda (value returnedState) (echo (list value (trimStateTo (callingLevel returnedState)))))
               ; Throw
               (lambda (exception thrownState) (echo (trimStateTo (callingLevel thrownState))))
               )
    )
  )

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
      ; If we're defining a function, do that
      [(eq? op 'function) (funcDeclare args state)]
      
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

      ; Helps in debugging
      [else (error "Unrecognized operator when progressing")]
      )
    )
  )