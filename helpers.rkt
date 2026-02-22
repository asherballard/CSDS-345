#lang racket
(provide (all-defined-out))

; Definitions related to syntax structure
(define primary (lambda (args) (car args)))
(define secondary (lambda (args) (cadr args)))
(define ternary (lambda (args) (caddr args)))
(define operator (lambda (node) (car node)))
(define argList (lambda (node) (cdr node)))
(define TRUE 'true)
(define FALSE 'false)

(define memberOf?
  (lambda (element lis)
    (if (eq? (indexof element lis) -1) #f #t)
    )
  )


; Returns the index of an element in a list, zero-indexed, or -1 if not found.
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

; For readability of cps functions
(define echo (lambda (v) v))

; Applys a function f to each element in a list
; Then returns the list
(define applyToEach
    (lambda (f lis)
        (if (null? lis) null
        (cons (f (car lis)) (applyToEach f (cdr lis)))
        )
      )
  )

; Returns the element in lis at index i (zero-indexed)
(define getElement
  (lambda (i lis return)
    (if (zero? i) (return (car lis))
    (getElement (+ i -1) (cdr lis) echo)
    )
    )
  )

; Makes a list '((x) (2)) instead of '((x) 2), for ease of state management
(define makePairedList (lambda (first second) (cons first (cons second null))))


; Removes the value at index in a list
(define cutSplice
  (lambda (index lis return)
    (if (zero? index) (return (cdr lis))
         (cutSplice (+ index -1) (cdr lis) (lambda (donelis) (return (cons (car lis) donelis))))
         )
    )
  )

;(define getValues
 ; (lambda (lis state return)
  ;  (cond
   ;   ((null? lis) lis) 
    ;  ((list? (car lis) (getValues (cdr lis) state (lambda (list) (return (cons list (lineParser (car lis) state)))))))
     ; ((or (bool? (car lis)) (number? (car lis))) (getValues (cdr lis) state (lambda (list) (return (cons list (car lis))))))
      ;(getValues (cdr lis) state (lambda (list) (return (cons list (lookupBinding (car lis) state))))))))

;(define condition
 ; (lambda (lis state)
  ;  (lineParser (car lis) state)))

;(define body
 ; (lambda (lis state)
  ;  (lineParser (cadr lis) state)))
