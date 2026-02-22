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
    (define combine (lambda (frontList backList) (cons frontList (cons backList null))))
    (define newNames (cons name (getNameList state)))
    (define newValues (cons value (getValueList state)))
    
    (combine newNames newValues)
    )
  )

; lookupBinding, returns a list of name and value; the name is null if the name isn't found
(define lookupBinding
  (lambda (name state)
    (define index (indexof name (getNameList state)))
    (if (eq? -1 index) (error "Error: Attempted to use undefined variable")
        (makePairedList name (getElement index (getValueList state) echo))
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
