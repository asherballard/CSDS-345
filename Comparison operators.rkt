#lang racket

(define equal
  (lambda (x y)
    (equal? x y)))

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