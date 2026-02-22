#lang racket


(define argument
  (lambda (lis)
    (car (car lis))))

(define lineParser
  (lambda (parseTree return)
    (cond
      ((eq? '+ (argument parseTree)) (addition (car parseTree)))
      ((eq? '- (argument parseTree)) (subtraction (car parseTree)))
      ((eq? '* (argument parseTree)) (multiplication (car parseTree)))
      ((eq? '/ (argument parseTree)) (division (car parseTree)))
      ((eq? '% (argument parseTree)) (modulo (car parseTree)))
      ((eq? '-- (argument parseTree)) (unary (car parseTree)))
      ((eq? '== (argument parseTree)) (equals (car parseTree)))
      ((eq? '!= (argument parseTree)) (notequal (car parseTree)))
      ((eq? '< (argument parseTree)) (greater (car parseTree)))
      ((eq? '> (argument parseTree)) (lesser (car parseTree)))
      ((eq? '<= (argument parseTree)) (lessorequal (car parseTree)))
      ((eq? '>= (argument parseTree)) (greaterorequal (car parseTree)))
      ((eq? '&& (argument parseTree)) (and (car parseTree)))
      ((eq? '|| (argument parseTree)) (or (car parseTree)))
      ((eq? '! (argument parseTree)) (not (car parseTree)))
      ((eq? 'var (argument parseTree)) (assign (car parseTree)))
      ((eq? '= (argument parseTree)) (assign (car parseTree)))
      ((eq? 'if (argument parseTree)) (if (car parseTree)))
      ((eq? 'while (argument parseTree)) (while (car parseTree)))
      ((eq? return (argument parseTree)) (return (car parseTree)))
      )))