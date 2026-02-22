#lang racket


(define addition
  (lambda (lis return)
    (cond
      ((null? lis) 0)
      ((null? (cdr lis)) (car lis))
      (addition (cdr lis) (lambda (sum) (return (+ sum (+ (car lis) (cdr lis)))))))))

(define subtraction
  (lambda (lis x return)
    (cond
      ((null? lis) 0)
      ((and (null? (cdr lis)) (eq? x 1)) (* -1 (car lis)))
      ((null? (cdr lis)) (car lis))
      (subtraction (cdr lis) 0 (lambda (difference) (return (- difference (car lis))))))))

(define multiplication
  (lambda (lis return)
    (cond
      ((null? lis) 0)
      ((null? (cdr lis)) (car lis))
      (multiplication (cdr lis) (lambda (product) (return (* product (* (car lis) (cdr lis)))))))))

(define division
  (lambda (lis return)
    (cond
      ((null? lis) 0)
      ((null? (cdr lis)) (car lis))
      (division (cdr lis) (lambda (quotient) (return (/ quotient (car lis))))))))

(define modulo*
  (lambda (lis return)
    (cond
      ((null? lis) 0)
      ((null? (cdr lis)) (car lis))
      (modulo* (cdr lis) (lambda (remainder) (return (modulo remainder (modulo (car lis) (cdr lis)))))))))
