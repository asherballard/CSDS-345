#lang racket
(provide (all-defined-out))

(define and*
  (lambda (lis return)
    (cond
      ((null? lis) #t)
      (and* (cdr lis) (lambda (v) (and v (car lis)))))))

(define or*
  (lambda (lis return)
    (cond
      ((null? lis) #f)
      (or* (cdr lis) (lambda (v) (or v (car lis)))))))

(define not
  (lambda (x)
    (not x)))
