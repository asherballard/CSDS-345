#lang racket
(provide (all-defined-out))

;conditionals

(define condition
  (lambda (lis state)
    (lineParser (car lis) state)))

(define body
  (lambda (lis state)
    (lineParser (cadr lis) state)))


(define else*
  (lambda (lis state)
    (cond
      (eq? 'if (car lis) (cdr (cdr (cdr lis))))
      (cdr (cdr lis)))))

(define if*
  (lambda (lis state)
    (cond
      ((eq? (car lis) 'return) (body lis state))
      ((condition lis state) (body lis state))
      (if* (else* lis state) state))))
      

(define while
  (lambda (lis state return)
    (cond
      ((condition lis state) (while lis state (lambda (loop) (lineParser (body lis) echo))))
      state)))



