#lang racket
(require "helpers.rkt")
(provide (all-defined-out))

; ==================
; STATE ABSTRACTIONS
; ==================


; Abstract the list of names and values
; For applying to a LAYER
(define getLayerNameList car)
(define getLayerValueList cadr)

; Abstract how a layer is interacted with
(define peekActiveLayer car)
(define stateHeritage cdr)
(define topLayerActive? (lambda (state) (null? (stateHeritage state))))
(define stateWithLayer (lambda (layer state) (cons layer state)))

; Abstract that represents "no value" (declaration without assignment)
(define EMPTY null)
(define isEMPTY? (lambda (val) (eq? val EMPTY)))

; Abstract the structure of the state
; The first 2 return updated LISTS
; NOTE: modified from part 1 to instead assume "list of layers" structure
(define newNameList (lambda (name state) (cons name (getLayerNameList (peekActiveLayer state)))))
(define newValueList (lambda (value state) (cons value (getLayerValueList (peekActiveLayer state)))))

(define state? list?)

; Shortcut to a "nothing declared" layer/state
(define voidLayer (makePairedList EMPTY EMPTY))
(define voidState (cons voidLayer null))

; Create an instance closure
(define instanceClosure (lambda (class valueList)
                          
                         ))

; Create a class closure
; All we need for this is the class body, which we can basically "interpret" and then grab
; the output state. However, we need to slightly modify the process by not calling evaluateExpression
; on variable declarations, and putting handling static declarations
; "fieldState" and "methodState" are accumulators, called with voidState initial values
(define classClosure (lambda (parentClass classBody) (classClosureInternal parentClass classBody voidState voidState)))
(define classClosureInternal (lambda (parentClass classBody fieldState methodState)

                       ; End of classBody, return the two states
                       (if (null? classBody) (list parentClass fieldState methodState)
                           ; Not end of classBody, process current line and recurse
                           ; Use lambda application for efficiency
                           ((lambda (statement op tail)
                              
                              
                              
                   
                              (cond
                                ; If function, add its closure to the methodState after checking for redeclare
                                [(eq? op 'function) (if (isLive? (primary (argList statement)) methodState)
                                                        (error "Function already declared in class scope")
                                                        (classClosureInternal parentClass tail fieldState (funcDeclare (argList statement) methodState))
                                                        )]

                                ; If variable assignment, add it to the field state
                                [(eq? 'var op) (if (isLive? (primary (argList statement)) fieldState)
                                                   (error "Variable already live")
                                                   (if (secondary? (argList statement))
                                                       ; If we have an expression, place it in
                                                       (classClosureInternal parentClass tail (declareAssign (primary (argList statement)) (secondary (argList statement)) fieldState) methodState)
                                                       ; Otherwise, just declare it
                                                       (classClosureInternal parentClass tail (declare (primary (argList statement)) fieldState) methodState)
                                                       )
                                                   )]
                           )
                       )
                            ; Lambda applied to:
                            (currentStatement classBody)
                            (operator (currentStatement classBody))
                            (remainingStatements classBody)
                            )
                           )
                       )
  )


; =============
; STATE HELPERS
; =============

; Not tail recursive, but doesn't have to be!
(define isDeclared?
  (lambda (name state)
    (cond
      [(null? state) #f]
      [(memberOf? name (getLayerNameList (peekActiveLayer state))) #t]
      [else (isDeclared? name (stateHeritage state))]
      )
    )
  )

(define isLive?
  (lambda (name state)
    (memberOf? name (getLayerNameList (peekActiveLayer state)))
    )
  )

(define trimStateTo
  (lambda (targetLength state)
    (define stateLength (length state))
    (define heightDifference (- stateLength targetLength))
    (if (eq? 0 heightDifference)
        state
        (matchLength (+ heightDifference -1) (tossActiveLayer state))
        )
    )
  )

(define matchLength
  (lambda (heightDifference state)
    (if (eq? heightDifference 0)
        state
        (matchLength (+ heightDifference -1) (tossActiveLayer state))
        )
    )
  )

; To shorten declaring functions
(define funcDeclare
  (lambda (args state)
    (define name (primary args))
    (define formalParams (secondary args))
    (define body (ternary args))
      
    (declareAssign name (createClosure formalParams body (length state)) state)
    )
  )


; ==================
; FUNCTION FUNCTIONS
; ==================

(define createEnvironment
  (lambda (actualParameters closure state)
    (addParameterLayer
     (getFormalParameters closure)
     actualParameters
     (trimStateTo (getScopeLevel closure) state))
    )
  )

(define addParameterLayer
  (lambda (formalParameters actualParameters state)
    (initializeNewLayer (makePairedList formalParameters actualParameters) state)
    )
  )

(define createClosure
  (lambda (formalParameters body scopeLevel)
    (list formalParameters body scopeLevel)
    )
  )

(define getBody
  (lambda (closure)
    (car (cdr closure))
    )
  )

(define getFormalParameters
  (lambda (closure)
    (car closure)
    )
  )

(define getScopeLevel
  (lambda (closure)
    (car (cdr (cdr closure)))
    )
  )

(define updateHeritage
  (lambda (callingState retState)
    ; The scope difference between the old state and new state
    (define callLength (length callingState))
    (define retLength (length retState))
    (define difference (- callLength retLength))

    ; Non-negative difference means the calling state is longer, so we update the returned layers
    (if (>= difference 0)
        (append (cutAfterN-cps callingState difference echo) retState)
        ; Negative difference means a longer returned state, so we safely trim excess layers
        (trimStateTo callLength retState)
        )
    )
  )



; =========================
; BINDING (STATE) FUNCTIONS
; =========================

; Essentially addBinding
; Adds the binding (name, value) to state
(define stateWith
  (lambda (name value state)
    (define updatedNames (newNameList name state))
    (define updatedValues (newValueList value state))
    (define updatedLayer (makePairedList updatedNames updatedValues))

    ; Check to make sure the name isn't taken already (should be redundant, but SOMEONE always finds a way)
    ; Note: this allows for redeclaration of a variable if it hasn't been declared IN THE CURRENT LAYER. To prevent clashes.
    (if (isLive? name state)
        (error "Variable name already declared in current scope")
        (stateWithLayer updatedLayer (stateHeritage state))
        )
    )
  )

; stateWithout, returns the state without the given variable
(define stateWithout
  (lambda (name state)
    (define index (indexof name (getLayerNameList (peekActiveLayer state))))

    (if (eq? -1 index)
        ; If the name isn't actually declared yet IN THIS SCOPE, we do nothing
        ; This is because stateWithout is only used during assignment, to clear any old binding IN THE SCOPE
        state


        ; Otherwise, make a new layer by splicing each list at it's index, and returning the new state
        ; Note that cutSplice is CPS recursive, and echo is shorthand for (lambda (v) v)
        (stateWithLayer
         (makePairedList
          (cutSplice index (getLayerNameList (peekActiveLayer state)) echo)
          (cutSplice index (getLayerValueList (peekActiveLayer state)) echo))
         (stateHeritage state))
        )
    )
  )

; lookupBinding, returns the elements value if found, errors otherwise
(define lookupBinding
  (lambda (name state)
    (define index (indexof name (getLayerNameList (peekActiveLayer state))))
    
    (if (eq? -1 index)
        ; If name isn't in state, check other layers. If still no, error
        (if (topLayerActive? state)
            (error "Attempted to access undeclared variable")
            
            ; This will error if necessary, which breaks.
            (lookupBinding name (stateHeritage state))
            )
        
        ; Else, check to see if it's undefined
        ; (with wacky notation to avoid calling getElement twice)
        ; Essentially this defines a lambda first, THEN returns that lambda applied to the binding's value.
        ; This is so we can store the value and not call getElement more than necessary.

        ; We don't need to check for bottom layer here, since we know the variable must be defined in this layer,
        ; if at all
        ((lambda (value) (if (isEMPTY? value)
            (error "Attempted to access undefined variable")
            value
            )) (getElement index (getLayerValueList (peekActiveLayer state)) echo))
        )
    )
  )

; Assigns value to name in the current state, and returns the mutated state
; NOTE: Variable must be declared somewhere in scope
(define assign
  (lambda (name value state)
    (if (isLive? name state)
        (stateWith name value (stateWithout name state))
        (initializeNewLayer (peekActiveLayer state) (assign name value (stateHeritage state)))
        )
    )
)

; Binds name to EMPTY, returns updated state
(define declare
  (lambda (name state)
    (if (memberOf? name (getLayerNameList (peekActiveLayer state)))
        (error "Variable re-declared")
        (stateWith name EMPTY state)
        )
    )
  )

; Combines declare and assign for readability
(define declareAssign
  (lambda (name value state)
    (assign name value (declare name state))
    )
  )

; Keeps stateWithout from being directly called
(define free
  (lambda (name state)
    (stateWithout name state)
    ))

; Simply adds a new layer to the front of the state
(define initializeNewLayer
  (lambda (layer state)
    (cons layer state)
    )
  )

; Removes the active layer
(define tossActiveLayer
  (lambda (state)
    (stateHeritage state)
    )
  )
