#lang racket
(require "helpers.rkt")
(provide (all-defined-out))

(define getNameList (lambda (state) (car state)))
(define getValueList (lambda (state) (cadr state)))
(define getName (lambda (binding) (car binding)))
(define getValue (lambda (binding) (cadr binding)))
(define voidState (makePairedList null null))


; Abstracts state structure away from interpreter
(define isDeclared?
  (lambda (name state)
    (memberOf? name (getNameList state))
    )
  )
                
(define stateWith
  (lambda (name value state)
    (define newNames (cons name (getNameList state)))
    (define newValues (cons value (getValueList state)))
    
    (makePairedList newNames newValues)
    )
  )

; lookupBinding, returns the elements value if found, errors otherwise
(define lookupBinding
  (lambda (name state)
    (define index (indexof name (getNameList state)))
    (if (eq? -1 index)
        (error "Error: attempted to access undeclared variable")
        
        ; Wacky notation to avoid calling getElement twice
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
    (if (eq? -1 index) state
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

; Handles variable declaration, either with null or value
; Currently unused to avoid calling evaluators within state functions
; (i.e. the state manager should only receive final values to avoid cross-contamination)
(define bindVariable
  (lambda (args state)
    (if (null? (cdr args))
        (declare (primary args) state)
        (assign (primary args) (secondary args) state))
    )
  )
