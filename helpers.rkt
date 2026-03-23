#lang racket
(provide (all-defined-out))
<<<<<<< Updated upstream
=======

; ==================
; SYNTAX ABSTRACTION
; ==================

; Abstract getting each argument from an argList
; For applying to an argList
(define primary car)
(define secondary cadr)
(define ternary caddr)

; For making an arbitrary statementList a block
(define makeBlock
  (lambda (statementList)
    (cons 'begin statementList)
    )
  )

; Abstracts statementList structure
(define addStatement (lambda (statement statementList) (cons statement statementList)))

; For applying to a node
(define primaryArg (lambda (statement) (primary (argList statement))))
(define secondaryArg (lambda (statement) (secondary (argList statement))))
(define ternaryArg (lambda (statement) (ternary (argList statement))))

; Abstract getting the operator and arguments from a node
; For applying to nodes
(define operator car)
(define argList cdr)

; Abstract boolean values
(define TRUE 'true)
(define FALSE 'false)

; Statement syntax abstraction
; For applying to statementList
(define remainingStatements cdr)
(define currentStatement car)


; Does argList have a next arg?
(define secondary?
  (lambda (args)
    (not (null? (cdr args)))
    )
  )

; Does argList have a next-next arg? (I.e. a third argument)
(define ternary?
  (lambda (args)
    (not (null? (cddr args)))
    )
  )

; Is atom a boolean literal? (I.e. TRUE or FALSE)
(define isBool?
  (lambda (atom)
    (or (eq? atom TRUE) (eq? atom FALSE))
    )
  )

; ================
; HELPER FUNCTIONS
; ================

; Is element a member of lis? Returns #t or #f
(define memberOf?
  (lambda (element lis)
    (if (eq? (indexof element lis) -1) #f #t)
    )
  )


; Returns the index of an element in a list, zero-indexed, or -1 if not found.
>>>>>>> Stashed changes
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

(define getValues
  (lambda (lis state return)
    (cond
      ((null? lis) lis) 
      ((list? (car lis) (getValues (cdr lis) state (lambda (list) (return (cons list (lineParser (car lis) state)))))))
      ((or (bool? (car lis)) (number? (car lis))) (getValues (cdr lis) state (lambda (list) (return (cons list (car lis))))))
      (getValues (cdr lis) state (lambda (list) (return (cons list (lookupBinding (car lis) state))))))))

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
