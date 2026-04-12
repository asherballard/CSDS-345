#lang racket
(require "state.rkt")
(provide (all-defined-out))

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
(define applyToEach-cps
    (lambda (f lis return)
      (if (null?  lis)
          (return null)
          (applyToEach-cps f (cdr lis) (lambda (donelis) (return (cons (f (car lis)) donelis))))
          )
      )
  )

; Returns the element in lis at index i (zero-indexed)
(define getElement
  (lambda (i lis return)
    (if (zero? i)
        (return (car lis))
        (getElement (+ i -1) (cdr lis) echo)
    )
    )
  )

; Makes a list '((x) (2)) instead of '((x) 2), for ease of state management
(define makePairedList (lambda (first second) (cons first (cons second null))))


; Removes the value at index in a list
; Then returns the list without that value
(define cutSplice
  (lambda (index lis return)
    (if (zero? index)
        (return (cdr lis))
        (cutSplice (+ index -1) (cdr lis) (lambda (donelis) (return (cons (car lis) donelis))))
         )
    )
  )

(define assignParams
  (lambda (formal actual state)
    (if (null? (secondary actual))
        (assignParamsHelper (primary formal) (primary actual) state)
        (assignParams (secondary formal) (secondary actual) state))))

(define assignParamsHelper
  (lambda (formal actual state)
    (if (isLive? formal state)
        (assign formal actual state)
        (assign formal actual (declare formal state)))))