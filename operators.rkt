#lang racket
(provide (all-defined-out))
(require "helpers.rkt")

; ===================
; SYNTAX CLASSIFIERS
; ====================

; Is this operator a conditional?
(define conditional?
  (lambda (operator)
    (define condList '(if while))
    (memberOf? operator condList)
    )
  )

; Classifies an operator as state-affecting or not, returns #t or #f
(define evolver?
  (lambda (operator)
    (define evolverList '(var =))
    (memberOf? operator evolverList)
    )
  )

; Classifies an operator as numerical or not, returns #t or #f
(define numerical?
  (lambda (operator)
    (define numericalList '(+ - * / %))
    (memberOf? operator numericalList)
    )
  )


; Classifies a bool-out operator as magnitude-based or not
; If so, returns #t, else, #f
(define magnitudeBased?
  (lambda (operator)
    (define comparativeList '(> < <= >=))
    (memberOf? operator comparativeList)
    )
  )

; Does the same as directly above, but for boolean-based operators
(define booleanBased?
  (lambda (operator)
    (define booleanList '(&& || !))
    (memberOf? operator booleanList)
    )
  )

; Returns if an operator outputs a boolean
(define boolOut?
  (lambda (operator)
    (or (magnitudeBased? operator) (booleanBased? operator))
    )
  )

; =====================
; PARSER INTERPRETATION
; =====================

; Takes a symbol and returns the matching procedure
(define convertOperator
  (lambda (op)
    (cond
      ; Ints in, ints out
      ((eq? '+ op) addition)
      ((eq? '- op) subtraction)
      ((eq? '* op) multiplication)
      ((eq? '/ op) division)
      ((eq? '% op) modulo*)

      ; Ints in, bools out
      [(eq? '== op) equal?]
      [(eq? '!= op) notEqual?]
      [(eq? '> op) greater?]
      [(eq? '< op) lesser?]
      [(eq? '<= op) lesserOrEqual?]
      [(eq? '>= op) greaterOrEqual?]

      ; Bools in, bools out
      [(eq? '&& op) and*]
      [(eq? '|| op) or*]
      [(eq? '! op) not*]

      ; These mutate bindings
      [(eq? 'var op) bindVariable]
      [(eq? '= op) bindVariable]

      ; These take conditions
      ;[(eq? 'if op) if*]
      ;[(eq? 'while op) while]

      ; Returns an  (condition or integer value)
      ;[(eq? 'return op) return]
      [else (error "Invalid operator")]
      )
    )
  )

; =================
; STATE OPERATORS
; =================
; These functions map state to state
(require "state.rkt")

; ======================
; INTEGER OPERATORS
; ======================
; All functions take in integer values, and return as such
; Essentially, integer maps

(define addition
  (lambda (args)
    (+ (primary args) (secondary args))
    )
  )

(define subtraction
  (lambda (args)
    (- (primary args) (secondary args))
    )
  )

(define multiplication
  (lambda (args)
    (* (primary args) (secondary args))
    )
  )
    

(define division
  (lambda (args)
    (quotient (primary args) (secondary args))
    )
  )

(define modulo*
  (lambda (args)
    (modulo (primary args) (secondary args))
    )
  )

; =================
; BOOLEAN OPERATORS
; =================
; All functions take in TRUE and FALSE, and return as such
; That is, these are boolean maps

(define and*
  (lambda (args)
    (if (and (eq? (primary args) TRUE) (eq? (secondary args) TRUE)) TRUE FALSE)
    )
  )

(define or*
  (lambda (args)
    (if (or (eq? (primary args) TRUE) (eq? (secondary args) TRUE)) TRUE FALSE)
    )
  )

(define not*
  (lambda (x)
    (if (eq? TRUE x) FALSE TRUE)
    )
  )


; ===============
; COMPARISON MAPS
; ===============
; These functions take in EITHER
; - two numbers
; - two bools (TRUE or FALSE as above)
; And produce a bool accoridngly

(define equal?
  (lambda (args)
    (if (eq? (primary args) (secondary args)) TRUE FALSE)
    )
  )

(define notEqual?
  (lambda (args)
    (not* (equal? args))
  )
)

; =================
; MAGNITUDE MAPS
; =================
; These functions take in integer values and produce a literal boolean (TRUE or FALSE, as defined above)
; That is, these are M_boolean

(define greater?
  (lambda (args)
      (if (> (primary args) (secondary args)) TRUE FALSE)
    )
  )

(define lesser?
    (lambda (args)
      (if (< (primary args) (secondary args)) TRUE FALSE)
      )
  )

  (define greaterOrEqual?
    (lambda (args)
      (not* (lesser? args))
      )
    )


  (define lesserOrEqual?
    (lambda (args)
      (not* (greater? args))
      )
    )

; =================
; CONDITIONALS
; ==================
; These mutate the state depending upon their condition

#|(define condition
  (lambda (lis state)
    (lineParser (car lis) state)))

(define body
  (lambda (lis state)
    (lineParser (cadr lis) state)))


(define if*
  (lambda (args state)
    (if (evaluateCond (primary args)) (runStatement)
      

(define while
  (lambda (lis state return)
    (cond
      ((condition lis state) (while lis state (lambda (loop) (lineParser (body lis) echo))))
      state)))
|#
