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
    (define args (argList node))
    (define op (operator node))
   (cond
     ; If the node is a declaration, check if re-declaered (error), then parse with either no value or an assignment
     [(eq? op 'var) (if (isDeclared? (primary args) state)
                        (error "Error: variables cannot be re-declared")
                        (if (null? (cdr args))
                            (declare (primary args) state)
                            (assign (primary args) (evaluateExpression (secondary args) state) state))
                        )]
     
     ; If the node is an assignment, first check that the variable exists
     [(eq? op '=) (if (isDeclared? (primary args) state)
                      (assign (primary args) (evaluateExpression (secondary args) state) state)
                      (error "Error: attempted to assign to undeclared variable")
                      )]
     
     ; If we're returning, do that
     [(eq? op 'return) (evaluateExpression (primary (argList node)) state)]
     
     ; If statement?
     [(eq? op 'if) (processIf (argList node) state)]

     ; Must be a while statement
     [else (processWhile (argList node) state)]
     
   )
 )
)

(define processIf
  (lambda (args state)
    (if (eq? (evaluateCondition (primary args) state) TRUE)
        (processStatement (secondary args) state)
        (if (null? (cddr args))
            state
            (processStatement (ternary args) state)
            )
        )
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
    (if (list? result)
        (stateProgress (cdr statementList) result)
        result
        )
  )
)

; The main interpret function
(define interpret
  (lambda (filename)
    (stateProgress (parser filename) voidState)
    )
  )
