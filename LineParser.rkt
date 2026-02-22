#lang racket
(require "helpers.rkt")
(require "state.rkt")
(require "comparisonOperators.rkt")
(require "booleanOperators.rkt")
(require "interpreterOperators.rkt")
(require "conditionalOperators.rkt")
(require "mathematicalOperators.rkt")

(define operator (lambda leaf (car leaf)))
(define argList (lambda leaf (cdr leaf)))

; Takes a statement and state
; Returns the updated state
;(define evaluateStatement
;  (lambda (statement state)
 ;   (cond
  ;    []
   ;   )
 ; )
;)

; Takes an integer value (can be an expression or variable name) and a state
; Returns a number
(define evaluateNum
  (lambda (node state)
    (cond
      [(and (null? (cdr node)) (number? (car node))) (car node)]
      [(null? (cdr node)) (lookupBinding (car node) state)]
      [else ((convertOperator (car node)) (applyToEach evaluateNum (cdr node)))]
      )
    )
  )

; Takes an atom and returns the matching procedure
(define convertOperator
  (lambda op
    (cond
      ; Return numbers
      ((eq? '+ op) addition)
      ((eq? '- op) subtraction)
      ((eq? '* op) multiplication)
      ((eq? '/ op) division)
      ((eq? '% op) modulo*)

      ; Return bools
      [(eq? '==' op) equals]
      [(eq? '!=' op) notequal]
      [(eq? '>' op) greater]
      [(eq? '<' op) lesser]
      [(eq? '<=' op) lesserOrEqual]
      [(eq? '>=' op) greaterOrEqual]
      [(eq? '&&' op) and*]
      [(eq? '||' op) or*]
      [(eq? '!' op) not*]

      ; Return states
      [(eq? 'var' op) assignNull]
      [(eq? '=' op) assign]
      [(eq? 'if' op) if*]
      [(eq? 'while' op) while]

      ; Returns an expression (condition or integer value)
      [(eq? 'return' op) return]
      )
    )
  )

; Takes a leaf and a state
; Returns a boolean
(define evaluateBool
  (lambda (leaf state)
    (cond
      ((eq? '== (operator parseTree)) (equals (car parseTree)))
      ((eq? '!= (operator parseTree)) (notequal (car parseTree)))
      ((eq? '< (operator parseTree)) (greater (car parseTree)))
      ((eq? '> (operator parseTree)) (lesser (car parseTree)))
      ((eq? '<= (operator parseTree)) (lessorequal (car parseTree)))
      ((eq? '>= (operator parseTree)) (greaterorequal (car parseTree)))
      ((eq? '&& (operator parseTree)) (and (car parseTree)))
      ((eq? '|| (operator parseTree)) (or (car parseTree)))
      ((eq? '! (operator parseTree)) (not (car parseTree)))
      )
    )
  )

; Takes a leaf and a state, and returns a new state
(define evaluateState
  (lambda (leaf state)
    (cond
      ((eq? 'var (operator parseTree)) (assign (car parseTree)))
      ((eq? '= (operator parseTree)) (assign (car parseTree)))
      ((eq? 'if (operator parseTree)) (if (car parseTree)))
      ((eq? 'while (operator parseTree)) (while (car parseTree)))
      ((eq? 'return (operator parseTree)) (return (car parseTree)))
    )
  )
)

;(define returnValue
 ; (lambda (expression state)
      
  ;  )
 ; )

; Takes a statement list and initial state, 
; updates the state based on the first statement,
; and when a return statement is reached, returns that value
(define stateProgress
  (lambda (statementList state)
      (define currentStatement (car statementList))

      (if (eq? (operator currentStatement) "return") (returnValue (argList currentStatement))
        (stateProgess (cdr statementList) (evaluateState (currentStatement statementList)))
    )
  )
)
