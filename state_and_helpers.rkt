#lang racket
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

(define getNameList (lambda (state) (car state)))
(define getValueList (lambda (state) (cadr state)))
(define getName (lambda (binding) (car binding)))
(define getValue (lambda (binding) (cadr binding)))

(define makePairedList (lambda (first second) (cons first (cons second null))))

(define cutSplice
  (lambda (index lis return)
    (if (zero? index) (return (cdr lis))
         (cutSplice (+ index -1) (cdr lis) (lambda (donelis) (return (cons (car lis) donelis))))
         )
    )
  )

; addBinding, but name shows you're supposed to use it as a value
(define stateWith
  (lambda (name value state)
    (define combine (lambda (frontList backList) (cons frontList (cons backList null))))
    (define newNames (cons name (getNameList state)))
    (define newValues (cons value (getValueList state)))
    
    (combine newNames newValues)
    )
  )

; lookupBinding, returns a list of name and value; the name is null if the name isn't found
(define lookupBinding
  (lambda (name state)
    (define index (indexof name (getNameList state)))
    (if (eq? -1 index) (cons null null)
        (makePairedList name (getElement index (getValueList state) echo))
        )
    )
  )

; stateWithout
(define stateWithout
  (lambda (name state)
    (define index (indexof name (getNameList state)))
    (if (eq? -1 index) state
        (makePairedList (cutSplice index (getNameList state) echo) (cutSplice index (getValueList state) echo))
        )
    )
  )