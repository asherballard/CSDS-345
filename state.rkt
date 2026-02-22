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
    (define newNames (cons name (getNameList state)))
    (define newValues (cons value (getValueList state)))
    
    (makePairedList newNames newValues)
    )
  )

; lookupBinding, returns the elements value if found, errors otherwise
(define lookupBinding
  (lambda (name state)
    (define index (indexof name (getNameList state)))
    (if (eq? -1 index) (error "Error: Attempted to use undefined variable")
        (getElement index (getValueList state) echo)
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
    (stateWith name null state)
    )
  )

; Handles variable declaration, either with null or value
(define bindVariable
  (lambda (args state)
    (if (null? (cdr args))
        (declare (primary (args)) state)
        (assign (primary (args)) (secondary (args)) state))
    )
  )
