#lang racket


(define addition
  (lambda (lis)
    (cond
      (and (number? 