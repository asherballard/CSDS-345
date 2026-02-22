#lang racket
(provide (all-defined-out))


(define addition
  (lambda (lis)
    (cond
      (and (number? 
