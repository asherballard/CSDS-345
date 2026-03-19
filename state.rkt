#lang racket
(require "helpers.rkt")
(provide (all-defined-out))

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
; Returns the updated LIST
; NOTE: modified from part 1 to instead assume "list of layers" structure
(define newNameList (lambda (name state) (cons name (getLayerNameList (peekActiveLayer state)))))
(define newValueList (lambda (value state) (cons value (getLayerValueList (peekActiveLayer state)))))
(define state? list?)

; Shortcut to a "nothing declared" state
(define voidState (makePairedList EMPTY EMPTY))


; Abstracts state structure away from interpreter
; Simply returns #t or #f, depending on whether name is in state's nameList
#|(define isDeclared?
  (lambda (name state)
    (memberOf? name (getLayerNameList (peekTopLayer state)))
    )
  )
|#

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

; =========================
; BINDING (STATE) FUNCTIONS
; =========================

; Essentially addBinding
; Adds the binding (name, value) to state
(define stateWith
  (lambda (name value state)
    (define newNames (newNameList name state))
    (define newValues (newValueList value state))
    (define newLayer (cons newNames newValues))

    ; Check to make sure the name isn't taken already (should be redundant, but SOMEONE always finds a way)
    ; Note: this allows for redeclaration WITHIN A LAYER. To prevent clashes.
    (if (isLive? name state)
        (error "Error: variable name already declared in current scope")
        (stateWithLayer newLayer (stateHeritage state))
        )
    )
  )

; stateWithout, returns the state without the given variable
(define stateWithout
  (lambda (name state)
    (define index (indexof name (getLayerNameList (peekActiveLayer state))))

    (if (eq? -1 index)
        ; If the name isn't actually declared yet IN THIS SCOPE, we do nothing
        state


        ; Otherwise, make a new state by splicing each list at it's index, and returning the new state
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
            (error "Error: attempted to access undeclared variable")
            
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
            (error "Error: attempted to access undefined variable")
            value
            )) (getElement index (getLayerValueList (peekActiveLayer state)) echo))
        )
    )
  )

; Assigns value to name in the current state, and returns the mutated state
(define assign
  (lambda (name value state)
    (stateWith name value (stateWithout name state))
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

; Simply adds a new voidState (really a layer, but I'm too lazy to change that rn) to the front of the state
(define initializeNewLayer
  (lambda (state)
    (makePairedList voidState state)
    )
  )

; Removes the active layer
(define tossActiveLayer
  (lambda (state)
    (stateHeritage state)
    )
  )
