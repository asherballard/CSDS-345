#lang racket
(provide (all-defined-out))
(define indexof
  (lambda (x lis)
   (define indexof-break
     (lambda (x lis break)
       (cond
         [(null? lis) (break -1)]
         [(eq? x (car lis)) 0]
         [else (+ 1 (indexof-break x (cdr lis) break))]
         )
       )
  )
   (call/cc (lambda (break)
       (indexof-break x lis break)
     )
    )
  )
)

(define echo (lambda (v) v))

(define getElement
  (lambda (i lis return)
    (if (zero? i) (return (car lis))
    (getElement (+ i -1) (cdr lis) echo)
    )
    )
  )

(define makePairedList (lambda (first second) (cons first (cons second null))))

(define cutSplice
  (lambda (index lis return)
    (if (zero? index) (return (cdr lis))
         (cutSplice (+ index -1) (cdr lis) (lambda (donelis) (return (cons (car lis) donelis))))
         )
    )
  )

