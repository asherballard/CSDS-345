#lang racket
(provide (all-defined-out))
(require "helpers.rkt")

; All functions take in 'true and 'false, and return as such
; That is, these are boolean maps
(define and*
  (lambda (args)
    (if (and (eq? (arg1 args) 'true) (eq? (arg2 args) 'true)) 'true 'false)
    )
  )

(define or*
  (lambda (args)
    (if (or (eq? (arg1 args) 'true) (eq? (arg2 args) 'true)) 'true 'false)
    )
  )

(define not*
  (lambda (x)
    (if (eq? 'true x) 'false 'true)
    )
  )
