#lang racket
(provide (all-defined-out))
(require "helpers.rkt")

(define addition
  (lambda (args)
    (+ (arg1 args) (arg2 args))
    )
  )

(define subtraction
  (lambda (args)
    (- (arg1 args) (arg2 args))
    )
  )

(define multiplication
  (lambda (args)
    (* (arg1 args) (arg2 args))
    )
  )
    

(define division
  (lambda (args)
    (quotient (arg1 args) (arg2 args))
    )
  )

(define modulo*
  (lambda (args)
    (modulo (arg1 args) (arg2 args))
    )
  )
