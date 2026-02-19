#lang racket


(define addition
  (lambda (x y)
    (+ x y)))

(define subtraction
  (lambda (x y)
    (- x y)))

(define multiplication
  (lambda (x y)
    (* x y)))

(define division
  (lambda (x y)
    (/ x y)))

(define unary
  (lambda (x)
    (* x -1)))

(define modulo
  (lambda (x y)
    (modulo x y)))
  