#lang racket


(define argument
  (lambda (lis)
    (car (car lis))))

(define lineParser
  (lambda (parseTree return)
    (cond
      ((eq? '+ (argument parseTree)) ())
      ((eq? '- (argument parseTree)) ())
      ((eq? '* (argument parseTree)) ())
      ((eq? '/ (argument parseTree)) ())
      ((eq? '% (argument parseTree)) ())
      ((eq? '-- (argument parseTree)) ())
      ((eq? '== (argument parseTree)) ())
      ((eq? '!= (argument parseTree)) ())
      ((eq? '< (argument parseTree)) ())
      ((eq? '> (argument parseTree)) ())
      ((eq? '<= (argument parseTree)) ())
      ((eq? '>= (argument parseTree)) ())
      ((eq? '&& (argument parseTree)) ())
      ((eq? '|| (argument parseTree)) ())
      ((eq? '! (argument parseTree)) ())
      ((eq? 'var (argument parseTree)) ())
      ((eq? '= (argument parseTree)) ())
      ((eq? 'if (argument parseTree)) ())
      ((eq? 'while (argument parseTree)) ())
      ((eq? return (argument parseTree)) ())
      )))