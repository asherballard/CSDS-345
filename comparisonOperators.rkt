#lang racket

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

  (define greaterorequal
    (lambda (lis return)
      (cond
        ((null? (cdr lis)) #t)
        ((< (car lis) (cdr lis)) (return #f))
        (greaterorequal (cdr lis) echo))))


  (define lessorequal
    (lambda (lis return)
      (cond
        ((null? (cdr lis)) #t)
        ((> (car lis) (cdr lis)) (return #f))
        (lessorequal (cdr lis) echo))))
