#lang racket
; This contains all expression evaluation functions (i.e., M_integer and M_bool)
(require "state.rkt")
(require "helpers.rkt")
(require "operators.rkt")
(provide (all-defined-out))


; Returns the appropriate evaluation of an expression (variable, literal, or nested expression)
; Input could be any expression
(define evaluateExpression
  (lambda (node state)
     (cond
       ; Is the expression a simple one? I.e. a number, boolean literal, or variable name
       [(number? node) node]
       [(symbol? node) (if (or (eq? node TRUE) (eq? node FALSE)) node (lookupBinding node state))]

       ; Ok, the expression is nested. Determine if it's numerical or conditional, and evaluate it as such.
       [(numerical? (operator node)) (evaluateNum node state)]
      [else (evaluateCondition node state)]
      )
    )
  )

; Takes a condition and a state
; Returns a boolean (TRUE or FALSE)
(define evaluateCondition
  (lambda (node state)
    (cond
      ; Is it a literal value?
      [(or (eq? node TRUE) (eq? node FALSE)) node]
      ; Is it a variable name?
      [(symbol? node) (lookupBinding node state)]
      ; It must be an operation
      [else (cond
             ; If magnitude-based, evaluate each arg as an integer
             [(magnitudeBased? (operator node))
                  ((convertOperator (operator node)) (applyToEach (lambda (x) (evaluateNum x state)) (cdr node)))]
                  
             ; If boolean-based, evaluate each arg as such
             [(booleanBased? (operator node))
                  ((convertOperator (operator node)) (applyToEach (lambda (x) (evaluateCondition x state)) (cdr node)))]
                  
             ; If we reach this point, it must be a comparison operator (== or !=). Thus we call the generic expression evaluator.
             [else ((convertOperator (operator node)) (applyToEach (lambda (x) (evaluateExpression x state)) (cdr node)))]
             )
       ]
      )
 )
)

; Takes an integer value (can be an expression or variable name) and a state
; Returns an integer
(define evaluateNum
  (lambda (node state)
    (cond
      ; Is it a number?
      [(number? node) node]
      ; Is it a variable name?
      [(symbol? node) (lookupBinding node state)]
      ; It must be an operation
      [else ((convertOperator (operator node)) (applyToEach (lambda (x) (evaluateNum x state)) (cdr node)))]
      )
    )
  )