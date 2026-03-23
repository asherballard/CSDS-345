#lang racket
; This contains all expression evaluation functions (i.e., M_value)
(require "state.rkt")
(require "helpers.rkt")
(require "operators.rkt")
(provide (all-defined-out))


; Returns the appropriate evaluation of an expression (variable, literal, or nested expression)
; Input could be any expression
(define evaluateExpression
  (lambda (node state)
     (cond
       ; ==========
       ; Is the expression a simple one? I.e. a number, boolean literal, or variable name

       ; If the node is a number, simple return
       [(number? node) node]

       ; If the node is a symbol, check if it's a boolean literal. If so, return the literal
       [(symbol? node) (if (isBool? node)
                           node
                           
                           ; If it isn't a boolean literal, it must be a variable
                           ; Return the variable's binding
                           (lookupBinding node state))]

       ; Ok, the expression is nested (not a literal or variable). Determine if it's numerical or conditional, and evaluate it as such.
       [(numerical? (operator node))(evaluateNum node state)]

       ; Must be a condition
      [else (evaluateCondition node state)]
      )
    )
  )

; Takes a condition and a state
; Returns a boolean (TRUE or FALSE)
(define evaluateCondition
  (lambda (node state)
    (cond
      ; Is it a bool literal? Then return it
      [(isBool? node) node]
      
      ; Is it a variable name? Return it's binding
      [(symbol? node) (lookupBinding node state)]
      
      ; It must be an operation
      [else (cond
              
             ; If magnitude-based, evaluate each arg as an integer, and return its value
             [(magnitudeBased? (operator node))
                  ((convertOperator (operator node)) (applyToEach-cps (lambda (x) (evaluateNum x state)) (argList node) echo))]

             ; Catch not, which only has one argument
             [(eq? '! (operator node))
              ((convertOperator (operator node)) (evaluateCondition (primary (argList node)) state))]
             
             ; If boolean-based, evaluate each arg as such, and return its value
             [(booleanBased? (operator node))
                  ((convertOperator (operator node)) (applyToEach-cps (lambda (x) (evaluateCondition x state)) (argList node) echo))]
                  
             ; If we reach this point, it must be a comparison operator (== or !=).
             ; Thus we call the generic expression evaluator, since these operators can be either numerical or a condition
             [else ((convertOperator (operator node)) (applyToEach-cps (lambda (x) (evaluateExpression x state)) (argList node) echo))]
             )
       ]
      )
 )
)

; Takes an integer value (can be an expression or a variable name) and a state
; Returns an integer
(define evaluateNum
  (lambda (node state)
    (cond
      ; Is it a number? Return it
      [(number? node) node]
      
      ; Is it a variable name? Return its binding
      [(symbol? node) (lookupBinding node state)]
      
      ; It must be an operation, apply the operation and return the value
      [else ((convertOperator (operator node)) (applyToEach-cps (lambda (x) (evaluateNum x state)) (argList node) echo))]
      )
    )
  )