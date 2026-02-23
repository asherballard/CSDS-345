#lang racket
(require "helpers.rkt")
(provide (all-defined-out))

; Abstract the list of names and values
(define getNameList (lambda (state) (car state)))
(define getValueList (lambda (state) (cadr state)))

; Abstract the retrieval of either part of a binding
(define getName (lambda (binding) (car binding)))
(define getValue (lambda (binding) (cadr binding)))

; Shortcut to a "nothing declared" state
(define voidState (makePairedList null null))


; Abstracts state structure away from interpreter
; Simply returns #t or #f, depending on whether name is in state's nameList
(define isDeclared?
  (lambda (name state)
    (memberOf? name (getNameList state))
    )
  )

; =================
; BINDING FUNCTIONS
; =================

; Essentially addBinding
; Adds the binding (name, value) to state
(define stateWith
  (lambda (name value state)
    (define newNames (cons name (getNameList state)))
    (define newValues (cons value (getValueList state)))

    ; Check to make sure the name isn't taken already (should be redundant, but SOMEONE always finds a way)
    (if (isDeclared? name state)
        (error "Error: variable name already declared")
        (makePairedList newNames newValues)
        )
    )
  )

; lookupBinding, returns the elements value if found, errors otherwise
(define lookupBinding
  (lambda (name state)
    (define index (indexof name (getNameList state)))
    
    (if (eq? -1 index)
        ; If name isn't in state, error
        (error "Error: attempted to access undeclared variable")
        
        ; Else, check to see if it's undefined
        ; (with wacky notation to avoid calling getElement twice)
        ; Essentially this defines a lambda first, THEN returns that lambda applied to the binding's value.\
        ; This is so we can store the value and not call getElement more than necessary.
        ((lambda (value) (if (null? value)
            (error "Error: attempted to access undefined variable")
            value
            )) (getElement index (getValueList state) echo))
        )
    )
  )

; stateWithout, returns the state without the given variable
(define stateWithout
  (lambda (name state)
    (define index (indexof name (getNameList state)))

    (if (eq? -1 index)
        ; If the name isn't actually declared yet, we do nothing
        state

        ; Otherwise, make a new state by splicing each list at it's index, and returning the new state
        ; Note that cutSplice is CPS recursive, and echo is shorthand for (lambda (v) v)
        (makePairedList (cutSplice index (getNameList state) echo) (cutSplice index (getValueList state) echo))
        )
    )
  )


; Assigns value to name in the current state, and returns the mutated state
(define assign
  (lambda (name value state)
    (stateWith name value (stateWithout name state))
    )
)

; Binds name to null, returns updated state
(define declare
  (lambda (name state)
    (if (memberOf? name (getNameList state))
        (error "Variable re-declared")
        (stateWith name null state)
        )
    )
  )