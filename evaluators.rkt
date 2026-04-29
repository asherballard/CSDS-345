#lang racket
; This contains all expression evaluation functions (i.e., M_value)
(require "state.rkt")
(require "helpers.rkt")
(require "operators.rkt")
(provide (all-defined-out))




(define getNonDotVariable (lambda (name state compileType)
                            (define compileClass (lookupBinding compileType state))
                            (cond
                              [(isDeclared? name state) (lookupBinding name state)]
                              [(isDeclared? name (getClassFieldState compileClass)) (fieldFromClass name compileType state)]
                              [else (error "undeclared variable")]
                              )
                            )
  )


; Takes in either an instance variable name, or a constructor node, and spits out the type
(define getType (lambda (instanceNode compileType state)
                  (cond
                    ; Check for super of the compileType
                    [(eq? 'super instanceNode) (primary (lookupBinding compileType state))]
                    ; Check for variable
                    [(symbol? instanceNode) (primary (lookupBinding instanceNode state))]
                    ; Must be constructor
                    [else (secondary instanceNode)]
                      )
                  )
  )

; Takes a node, state, and relevant types, and retrieves a function closure
; Takes in either simply a function name or a (dot {instance} {funcName})
(define getFunctionClosure (lambda (node state compileType throw return)
                            (if (symbol? node)
                                ; Call without dot, get the closure from the compileType
                                (lookupBinding node (getClassMethodState (lookupBinding compileType state)))

                                ; Call with dot, get the closure from the calling type
                                ; Lookup the .func
                                (lookupBinding (ternary node)
                                               ; In the class method state
                                               (getClassMethodState
                                                ; From looking up the class of the instance
                                                ((lambda (type) (lookupBinding type state))
                                                (getType (secondary node) compileType state))
                                                )
                                               )
                                )
                            )
  )

; For handling "x" "(new X)" "this" and "super"
; Return is of the format '(classname, instanceClosure)
(define dotInstance (lambda (dotNode state throw return compileType)
                      (define node (secondary dotNode))
                           (cond
                             
                             ; For (dot super X), we lookup the compileType's closure to get the parent, then make it a list like (super) to pass as an empty constructor
                             [(eq? 'super node) (emptyConstructor (primary (lookupBinding compileType state)) state (lambda (instanceClosure state) (list node instanceClosure)))]

                             ; For (dot this f), we simply fetch "this" from the state
                             [(eq? 'this node) ((lambda (thisClosure) (list 'this thisClosure)) (lookupBinding 'this state))]

                             ; For any other symbol, it must be a variable name
                             [(symbol? node) (list node (lookupBinding node state))]

                             ; For (new X), we call the constructor
                             [else (callConstructor (argList node) state throw (lambda (instanceClosure state) (list (secondary node) instanceClosure)))]
                               )
                           )
  )

; Creates a searchable state from an instance and it's class
(define instanceState (lambda (instance state)
                        (define valueList (instanceValues instance))
                        (define classClosure (lookupBinding (instanceClass instance) state))
                        (define fieldList (getClassFieldList classClosure))
                        (initializeNewState (list fieldList valueList))
                        )
  )

; Returns an updated instance
(define updateField (lambda (fieldName value instance state)
                      (define instState (instanceState instance state))
                      (define newInstState (assign fieldName value instState))
                      (instanceClosure (instanceClass instance) (getLayerValueList (peekActiveLayer newInstState)))
                      )
  )

; Returns the value of an instance field
(define getFieldValue (lambda (fieldName instance state)
                        (define instState (instanceState instance state))
                        (lookupBinding fieldName instState)
                        )
  )


; Takes in an initial state, a operational node, and the throw and return continuations
; Evaluates the node's arguments and updates the state with side effects,
; then if nothing was thrown, applies the operator to the evaluated arguments
; and returns the value and updated state
(define processOperation
  (lambda (state node throw return compileType callType)
    (processArgs null state (argList node) throw
                 (lambda (evalArgs evalState)
                   (return ((convertOperator (operator node)) evalArgs) evalState)
                   )
                 compileType
                 callType
                 )
    )
  )


; Takes in a list of evaluated arguments (typically null on first call), an initial state,
; unprocessed arguments (formal parameters), and the throw and return continuations.
; If nothing is thrown, returns the actual parameters and the updated with any side-effects state
(define processArgs
  (lambda (evald state args throw return compileType callType)
    (if (null? args)
        (return evald state)
        (evaluateExpression (car args) state throw
                            (lambda (val retState)
                              (processArgs (append evald (list val))
                                           retState
                                           (cdr args)
                                           throw
                                           return
                                           compileType
                                           callType)
                              )
                            compileType
                            callType
                            )
        )
    )
  )


; Returns the appropriate evaluation of an expression (variable, literal, object, or nested expression)
; Input could be any expression
; To allow for side-effects, will also return a state as the 2nd element in a list
(define evaluateExpression
  (lambda (node state throw return compileType callingInstance)
     (cond
       ; ==========
       ; Is the expression a simple one? I.e. a number, boolean literal, or variable name

       ; If the node is a number, simple return
       [(number? node) (return node state)]

       ; If the node is a symbol, check if it's a boolean literal. If so, return the literal
       [(symbol? node) (if (isBool? node)
                           (return node state)
                           
                           ; If it isn't a boolean literal, it must be a variable
                           ; Return the variable's binding from the compileType (no dot operator)
                           (return (getNonDotVariable node state compileType) state))]

       ; If the node is null, we're evaluating an undeclared field, so we keep it empty and catch the error later
       [(eq? EMPTY node) (return node state)]

       ; Ok, the expression is nested (not a literal or variable). Is it numerical?
       [(numerical? (operator node)) (evaluateNum node state throw return compileType callingInstance)]

       ; If this is a function call, check if the function is local or a dot access.
       [(eq? 'funcall (operator node))
        ((lambda (notDotCall)
         (callFunction (getFunctionClosure (primary (argList node)) state compileType throw return) (cdr (argList node)) state throw return (if notDotCall
                                                                                                                                                callingInstance
                                                                                                                                                (dotInstance (primary (argList node)) state throw (lambda (instance state) instance) compileType)
                                                                                                                                                ))
         ) (symbol? (secondary node))
           )]

       ; Is it a dot operation? It must be accessing a field value
       [(eq? 'dot (operator node)) ((lambda (instState)
                                      (if (isLive? (ternary node) instState)

                                       ; It's an instance value, return it from there
                                      (return (lookupBinding (secondary (argList node)) instState) state)
                                      
                                      ; It's not an instance value, so it must be a static value
                                      (error "I'm not doing static fields")
                                       )) (instanceState (secondary (dotInstance node state throw return compileType)) state))]
       
       ; Is it a constructor? Then call it
       [(eq? 'new (operator node)) (callConstructor (argList node) state throw return)]

       ; Must be a condition
      [else (evaluateCondition node state throw return compileType callingInstance)]
      )
    )
  )




; Takes a condition and a state
; Returns a boolean (TRUE or FALSE)
(define evaluateCondition
  (lambda (node state throw return compileType callingInstance)
    (cond
      ; Is it a bool literal? Then return it
      [(isBool? node) (return node state)]
      
      ; Is it a variable name? Return it's binding
      [(symbol? node) (return (fieldFromClass node compileType state) state)]
      
      ; It must be an operation
      [else (processOperation state node throw return compileType callingInstance)]
      )
 )
)

; Takes an integer value (can be an expression or a variable name) and a state
; Returns an integer
(define evaluateNum
  (lambda (node state throw return compileType callingInstance)
    (cond
      ; Is it a number? Return it
      [(number? node) (return node state)]
      
      ; Is it a variable name? Return its binding
      [(symbol? node) (return (fieldFromClass node compileType state) state)]
      
      ; It must be an operation, apply the operation and return the value
      [else (processOperation state node throw return compileType callingInstance)]
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
  (lambda (closure actualParameters state throw return callingInstance)
    (define formalParameters (primary closure))
    (define isDynamic (and (not (null? formalParameters))(eq? (getLastElement formalParameters) 'this)))
    (define callingInstanceName (if isDynamic (primary callingInstance) null))
    (define callingInstanceClosure (if isDynamic (secondary callingInstance) null))
    (define classClosure (if isDynamic (lookupBinding (instanceClass (secondary callingInstance)) state) null))
    (define compileType (quaternary closure))
    (define callingLevel (length state))

    ; Call the statementList evaluator with the environment on the body
    ; Note: return will provide a list where the 1st element is the returned value,
    ; and the 2nd is the environment at the time
    (processArgs null state actualParameters throw (lambda (evalArgs evalState)
                                                     (define caller (if isDynamic
                                                                        callingInstance
                                                                             null))
                                                     (define dynamicArgs (if isDynamic
                                                                             (append evalArgs (list callingInstanceClosure))
                                                                             evalArgs
                                                                             )
                                                       )
                                                            (nextState (createEnvironment dynamicArgs closure evalState caller) (getBody closure)
                                                                       ; Next
                                                                       echo
                                                                       ; Break
                                                                       (lambda (brokenState) (error "Break outside of a loop"))
                                                                       ; Continue
                                                                       (lambda (continuedState) (error "Continue outside of a loop"))
                                                                       ; Return
                                                                       (lambda (value returnedState)
                                                                         (define updatedThis (if isDynamic (lookupBinding callingInstanceName returnedState) null))
                                                                         (define cleanReturn (if isDynamic (tossActiveLayer returnedState) returnedState))
                                                                         (define updatedThisState (if isDynamic (assign callingInstanceName updatedThis state) state))
                                                                         (return value (updateHeritage updatedThisState cleanReturn))
                                                                         )
                                                                       ; Throw
                                                                       (lambda (exception thrownState)
                                                                        ; (define updateThis (assign callingInstanceName (lookupBinding 'this thrownState) thrownState))
                                                                         (throw exception (updateHeritage state thrownState))
                                                                         )
                                                                       compileType
                                                                       caller
                                                                       )
                                                     )
                 compileType
                 callingInstance
                 )
    )
  )

; =======
; CLASSES
; =======

; Finds the relevant constructor for a class in the state, calls it,
; and returns (Object, side-affected state)
(define callConstructor
  (lambda (args state throw return)
    (define targetClass (lookupBinding (primary args) state))
    (define fieldValues (remaining args))
    (define fieldState (getClassFieldState targetClass))
    (define constructorState (getClassConstructors targetClass))
    (if (isLive? (length fieldValues) constructorState)
        ; Non-empty constructor: not properly implemented but optional
        (callFunction (lookupBinding (length fieldValues) constructorState) fieldValues state throw return)
        ; Empty constructor: instance with no values
        (emptyConstructor (primary args) state return)
        )
    )
  )

; Turns an instance of a child into an instance of a parent, by calling the implicit constructor and updating fields accordingly
(define polymorph (lambda (childInstanceClosure state)
                    (define trueName (primary childInstanceClosure))
                    (define parentClosure (lookupBinding trueName state))
                    (define parentName (primary parentClosure))
                    (define parentFields (getClassFieldList parentClosure))
                    (define implicitParentClosure (emptyConstructor parentName state (lambda (instance state) instance)))
                    0
                    )
  )

  ; Returns an instance with the default values
(define emptyConstructor (lambda (className state return)
                           (define classClosure (lookupBinding className state))
                           (define fieldsState (getClassFieldState classClosure))
                           (define layer (peekActiveLayer fieldsState))
                           (define names (getLayerNameList layer))
                           (define expressions (getLayerValueList layer))
                           (define emptySuper (if (eq? (getClassParent classClosure) 'Object) null (emptyConstructor (getClassParent classClosure) state (lambda (instance state) instance))))
                           (emptyConstructorRec (list className) names expressions state return)
                           )
  )
(define emptyConstructorRec (lambda (instance names expressions state return)
                              (cond
                                [(null? names) (return instance state)]
                                [(isLive? (car names) state) (error "Field value with same name")]
                                [else (evaluateExpression (car expressions) state list (lambda (val nState)
                                                                                         (add2end-cps val instance (lambda (newInstance)
                                                                                                                     (emptyConstructorRec newInstance (cdr names) (cdr expressions) nState return)
                                                                                                                     ))
                                                                                         ) (car instance) (car instance))]
                                )
                              )
  )
                              

 (define processFieldState (lambda (fieldState)
                             (define layer (peekActiveLayer fieldState))
                             (define names (getLayerNameList layer))
                             (define expressions (getLayerValueList layer))
                             (processFieldStateRec names expressions null)
                             )
   )
  (define processFieldStateRec (lambda (names expressions statementList)
                                 (if (null? names)
                                     statementList
                                     (processFieldStateRec (cdr names) (cdr expressions) (cons (cons 'var (list (car names) (car expressions))) statementList))
                                     )
                                 )
    )

; Handles assignment statements for nextState
(define assignState (lambda (target expression state throw tail break continue return compileType callingInstance)
                      ; Determine if variable is dotted or not
                      (if (list? target)
                          ; If the target is a list, it's of the form (dot instance fieldName)
                          ; Thus check for instance, then it's fields
                          ((lambda (targetDeclared)
                             (if targetDeclared
                                 (evaluateExpression expression state throw (lambda (val retState)
                                                                                       (define nState (assign (secondary target) (updateField (ternary target) val (lookupBinding (secondary target) retState) retState) retState))
                                                                                       (nextState nState tail echo break continue return throw compileType callingInstance)
                                                                                       )
                                                     compileType
                                                     callingInstance)
                                 (error "Variable undeclared")
                                 )
                             )
                           (isDeclared? (secondary target) state)
                          )
                          
                          ; If the target isn't a list (i.e. not (dot x x))
                          ; we proceed as normal
                          (if (isDeclared? target state)
                              (evaluateExpression expression state throw (lambda (val retState)
                                                                                    (define nState (assign target val retState))
                                                                                    (nextState nState tail echo break continue return throw compileType callingInstance)
                                                                                    )
                                                  compileType
                                                  callingInstance)
                              (error "Variable undeclared")
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
  (lambda (state statementList next break continue return throw compileType callingInstance)
    
    (define statement (if (null? statementList) null (currentStatement statementList)))
    (define op (operator statement))
    (define args (argList statement))
    (define newState (next state))
    (define testTail (remainingStatements statementList))
    ; Default return
    (define tail (if (null? testTail) (list '(return 0)) testTail))
    
    (cond
      ; If we're defining a function, do that
      [(eq? op 'function) (nextState (funcDeclare args newState) tail echo break continue return throw compileType callingInstance)]

      ; If we're calling a function without an assignment, update the state accordingly
      ; If the function hasn't been declared yet, we must be inside another function.
      ; Thus, since we need to save our place for proper state machining, but we also need to know the function
      ; to update the state properly, we skip ahead to find the function before declaring it, then proceeding.
      
      [(eq? op 'funcall) ((lambda (notDotCall)
                            (define funcState (if notDotCall
                                                  (if (isDeclared? (primary args) newState)
                                                      newState
                                                      (findFunction (primary args) tail newState)
                                                      )
                                                  newState
                                                  )
                              )
                            (callFunction (getFunctionClosure (primary args) state compileType throw return) (cdr args) funcState throw (lambda (value retState)
                                                                                                                                           (nextState retState tail echo break continue return throw compileType callingInstance)
                                                                                                                                           )
                            (if notDotCall
                                callingInstance
                                (dotInstance (primary args) state throw (lambda (instance state) instance) compileType)
                                ))
                            )
                          (symbol? (primary args))
                          )]
      
      ; If we're throwing, do that after tossing the try block scope
      [(eq? op 'throw) (evaluateExpression (primary args) newState
                                           echoDouble
                                           (lambda (exception thrownState)
                                             (throw exception (tossActiveLayer thrownState))
                                             )
                                           compileType
                                           callingInstance
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
                                                  compileType
                                                  callingInstance
                                                  ))
                                     compileType
                                     callingInstance)]

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
                                  compileType
                                  callingInstance
                                  ))
                     compileType
                     callingInstance)]

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
                                  compileType
                                  callingInstance
                                  ))
                     compileType
                     callingInstance)]
           )
       ]
      
      ; Check for end of block
      [(eq? op 'end) (nextState (tossActiveLayer newState) tail echo break continue return throw compileType callingInstance)]

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

                                  throw

                                  compileType
                                  callingInstance)]
      
      ; If we're returning, do that
      [(eq? 'return op) (evaluateExpression (primary (argList statement)) newState throw
                                            (lambda (value retState)
                                              (define isStatic (null? callingInstance))
                                              (define thisValue (if isStatic null (lookupBinding 'this retState)))
                                              (define cleanState (tossActiveLayer retState))
                                              ; throw on a "this" only layer to be scrubbed by callFunction later
                                              ; but only if the function isn't static
                                              (define tempLayer (if isStatic cleanState (declareAssign (primary callingInstance) thisValue (initializeNewLayer voidLayer cleanState))))
                                              (return value tempLayer)
                                              )
                                            compileType
                                            callingInstance
                                            )
                        ]
      
      ; If we're assigning or declaring, pass that into next
      [(eq? 'var op) (if (isLive? (primary (argList statement)) newState)
                         (error "Variable already live in environment")
                         (if (secondary? args)
                             (evaluateExpression (secondary args) newState throw
                                                 (lambda (val retState)
                                                   (define nState (declareAssign (primary args) val retState))
                                                   (nextState nState tail echo break continue return throw compileType callingInstance)
                                                   )
                                                 compileType
                                                 callingInstance
                                                 )
                             (nextState (declare (primary args) newState) tail echo break continue return throw compileType callingInstance)
                             )
       )]

      [(eq? '= op) (assignState (primary (argList statement)) (secondary (argList statement)) newState throw tail break continue return compileType callingInstance)]
      
      #|[(eq? '= op) (if (isDeclared? (primary (argList statement)) newState)
                       (evaluateExpression (secondary args) newState throw (lambda (val retState)
                                                                             (define nState (assign (primary args) val retState))
                                                                             (nextState nState tail echo break continue return throw)
                                                                             )
                                           compileType
                                           callType)
                       (error "Variable undeclared")
                       )]|#

      ; If it's an if statement, evaluate the condition and apply next appropriately
      [(eq? op 'if) (if (eq? (evaluateExpression (primary args) newState throw (lambda (val retState) val) compileType callingInstance) TRUE)
                        ; True condition means we put the first statement on the statementList
                        (nextState newState (addStatement (secondary args) tail) echo break continue return throw compileType callingInstance)

                        ; Check for an else condition, put it on the list if exists
                        (if (ternary? args)
                            (nextState newState (addStatement (ternary args) tail) echo break continue return throw compileType callingInstance)
                            (nextState newState tail echo break continue return throw compileType callingInstance)
                            )
                        )]

      ; If we break or continue, jump out with the newState
      [(eq? op 'break) (break newState)]
      [(eq? op 'continue) (continue newState)]

      ; If it's a while statement, keep reprocessing the statement until the condition is false or we break
      [(eq? op 'while) (if (eq? (evaluateExpression (primary args) newState throw (lambda (val retState) val) compileType callingInstance) TRUE)
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
                                      throw
                                      compileType
                                      callingInstance)

                           ; If condition is false, do nothing
                           (nextState newState tail echo break continue return throw compileType callingInstance)
                           )]

      ; Helps in debugging
      [else (error "Unrecognized operator when progressing")]
      )
    )
  )