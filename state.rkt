#lang racket
(require "helpers.rkt")
(provide (all-defined-out))

(define getNameList (lambda (state) (car state)))
(define getValueList (lambda (state) (cadr state)))
(define getName (lambda (binding) (car binding)))
(define getValue (lambda (binding) (cadr binding)))
(define voidState (makePairedList null null))

; addBinding, but name shows you're supposed to use it as a value
(define stateWith
  (lambda (name value state)
<<<<<<< Updated upstream
    (define combine (lambda (frontList backList) (cons frontList (cons backList null))))
    (define newNames (cons name (getNameList state)))
    (define newValues (cons value (getValueList state)))
    
    (combine newNames newValues)
=======
    (define updatedNames (newNameList name state))
    (define updatedValues (newValueList value state))
    (define updatedLayer (makePairedList updatedNames updatedValues))

    ; Check to make sure the name isn't taken already (should be redundant, but SOMEONE always finds a way)
    ; Note: this allows for redeclaration of a variable if it hasn't been declared IN THE CURRENT LAYER. To prevent clashes.
    (if (isLive? name state)
        (error "Variable name already declared in current scope")
        (stateWithLayer updatedLayer (stateHeritage state))
        )
>>>>>>> Stashed changes
    )
  )

; lookupBinding, returns a list of name and value; the name is null if the name isn't found
(define lookupBinding
  (lambda (name state)
<<<<<<< Updated upstream
    (define index (indexof name (getNameList state)))
    (if (eq? -1 index) (error "Error: Attempted to use undefined variable")
        (makePairedList name (getElement index (getValueList state) echo))
=======
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
>>>>>>> Stashed changes
        )
    )
  )

; stateWithout
(define stateWithout
  (lambda (name state)
    (define index (indexof name (getNameList state)))
    (if (eq? -1 index) state
        (makePairedList (cutSplice index (getNameList state) echo) (cutSplice index (getValueList state) echo))
        )
    )
  )
<<<<<<< Updated upstream
=======

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
>>>>>>> Stashed changes
