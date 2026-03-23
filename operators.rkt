#lang racket
(provide (all-defined-out))
(require "helpers.rkt")

; ===================
; SYNTAX CLASSIFIERS
; ====================

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

; =====================
; PARSER INTERPRETATION
; =====================

; Takes a symbol and returns the matching procedure
; This is to condense notation for expression evaluation
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

      [else (error "Invalid operator")]
      )
    )
  )

; ======================
; INTEGER OPERATORS
; ======================
; All functions take in integer values, and return as such
; Essentially, integer maps (M_value)

(define addition
  (lambda (args)
    (+ (primary args) (secondary args))
    )
  )

; Here we handle the slightly more complicated case of "is it a unary negative?"
(define subtraction
  (lambda (args)
    ; Is it a unary call?
    (if (null? (cdr args))
        ; If so, for input x we return x - 2x = -x
        (- (primary args) (* 2 (primary args)))
        
        ;Otherwise, do normal subtraction
        (- (primary args) (secondary args))
        )
    )
  )

(define multiplication
  (lambda (args)
    (* (primary args) (secondary args))
    )
  )
    

(define division
  (lambda (args)
    ; Use quotient so that "/" returns integer values only (drops any fractional part)
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
; All functions take in TRUE and FALSE (abstractions defined in helpers.rkt), and return as such
; That is, these are boolean maps (Still M_value)

(define and*
  (lambda (args)
    (if (and
         (eq? (primary args) TRUE)
         (eq? (secondary args) TRUE))
        TRUE
        FALSE
        )
    )
  )

(define or*
  (lambda (args)
    (if (or
         (eq? (primary args) TRUE)
         (eq? (secondary args) TRUE))
        TRUE
        FALSE
        )
    )
  )

(define not*
  (lambda (x)
    (if (eq? TRUE x)
        FALSE
        TRUE
        )
    )
  )


; ===============
; COMPARISON MAPS
; ===============
; These functions take in EITHER
; - two numbers
; - two bools (TRUE or FALSE as in helpers.rkt)
; And produce a bool accoridngly
; Thus M_value

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
; These functions take in integer values and produce a literal boolean (TRUE or FALSE, as defined in helpers.rkt)
; Still M_value

(define greater?
  (lambda (args)
      (if (> (primary args) (secondary args))
          TRUE
          FALSE
          )
    )
  )

(define lesser?
    (lambda (args)
      (if (< (primary args) (secondary args))
          TRUE
          FALSE
          )
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