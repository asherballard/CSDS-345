#lang racket
(require "helpers.rkt")
(require "state.rkt")
(require "operators.rkt")
(require "evaluators.rkt")
(require "simpleParser.rkt")
(provide (all-defined-out))

; Takes a statement and state
; Returns the updated state, or a value if return is called
(define processStatement
  (lambda (node state)
   (cond
     ; If the node is a declaration, parse with either no value or an assignment
     [(evolver? (operator node)) (if (null? (cdr (argList node))) (declare (primary (argList node)) state) (assign (primary (argList node)) (evaluateExpression (secondary (argList node)) state) state))]
     
     ; If we're returning, do that
     [(eq? (operator node) 'return) (evaluateExpression (primary (argList node)) state)]
     
     ; If statement?
     [(eq? (operator node) 'if) (processIf (argList node) state)]

     ; Must be a while statement
     [else (processWhile (argList node) state)]
     
   )
 )
)

(define processIf
  (lambda (args state)
    (if (eq? (evaluateCondition (primary args) state) TRUE) (processStatement (secondary args) state)
        (if (null? (cddr args)) state (processStatement (ternary args) state)))
  )
)

(define processWhile
  (lambda (args state)
    (if (eq? (evaluateCondition (primary args) state) TRUE)
        (processWhile args (processStatement (secondary args) state))
        state
    )
  )
)

; Takes a statement list and initial state, 
; updates the state based on the first statement,
; and when a return statement is reached (i.e. processStatement returns not a list), returns that value
; I.e. THIS SHOULD ONLY RETURN VALUES!!!
(define stateProgress
  (lambda (statementList state)
    (define currentStatement (car statementList))
    (define result (processStatement currentStatement state))
    (if (list? result) (stateProgress (cdr statementList) result) result)
  )
)

; The main interpret function
(define interpret
  (lambda (filename)
    (stateProgress (parser filename) voidState)
    )
  )
