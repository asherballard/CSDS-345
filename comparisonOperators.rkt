#lang racket
(provide (all-defined-out))

(define equal
  (lambda (lis return)
    (cond
      ((null? lis) #t)
      (equal (cdr lis) (lambda (equals) (return 

(define notequal
  (lambda (x y)
    (not (equal? x y))))

(define greater
  (lambda (x y)
    (> x y)))

(define lesser
  (lambda (x y)
    (< x y)))

(define greaterorequal
  (lambda (x y)
    (not (lesser x y))))

(define lessorequal
  (lambda (x y)
    (not (greater x y))))
