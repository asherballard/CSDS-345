#lang racket
(require "helpers.rkt")
(provide (all-defined-out))

(define equal
  (lambda (lis return)
    (cond
      ((null? (cdr lis)) #t)
      ((not (eq? (car lis) (cdr lis))) (return #f))
      (equal (cdr lis) echo))))

(define notequal
  (lambda (lis return)
    (cond
      ((null? (cdr lis)) #t)
      ((eq? (car lis) (cdr lis))) (return #f)
      (notequal (cdr lis) echo)))
  )

  (define greater
    (lambda (lis return)
      (cond
        ((null? (cdr lis)) #t)
        ((not (> (car lis) (cdr lis))) (return #f))
        (greater (cdr lis) echo))))


  (define lesser
    (lambda (lis return)
      (cond
        ((null? (cdr lis)) #t)
        ((not (< (car lis) (cdr lis))) (return #f))
        (lesser (cdr lis) echo))))

  (define greaterOrEqual
    (lambda (lis return)
      (cond
        ((null? (cdr lis)) #t)
        ((< (car lis) (cdr lis)) (return #f))
        (greaterOrEqual (cdr lis) echo))))


  (define lesserOrEqual
    (lambda (lis return)
      (cond
        ((null? (cdr lis)) #t)
        ((> (car lis) (cdr lis)) (return #f))
        (lesserOrEqual (cdr lis) echo))))
