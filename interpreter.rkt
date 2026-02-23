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
     ; If the node is a declaration, check if its a redeclaration (error), then parse with either no value or an assignment as appropriate
     [(eq? op 'var) (if (isDeclared? (primary args) state)
                        (error "Error: variables cannot be re-declared")
                        (if (secondary? args)
                            (assign (primary args) (evaluateExpression (secondary args) state) state)
                            (declare (primary args) state)
                        )
                     )]
     
     ; If the node is an assignment, first check that the variable exists
     [(eq? op '=) (if (isDeclared? (primary args) state)
                      (assign (primary args) (evaluateExpression (secondary args) state) state)
                      (error "Error: attempted to assign to undeclared variable")
                      )]
     
     ; If we're returning, do that after evaluating the expression
     [(eq? op 'return) (evaluateExpression (primary (argList node)) state)]
     
     ; If statement? Move to relevant processor and return that
     [(eq? op 'if) (processIf (argList node) state)]

     ; Must be a while statement, move to *that* processor
     [else (processWhile (argList node) state)]
     
   )
 )
)


; Take in the args of an if construct
; Returns the updated state from evaluating it
(define processIf
  (lambda (args state)
    ; If the condition evaluates to TRUE, pass the state given by processing statement 1
    (if (eq? (evaluateCondition (primary args) state) TRUE)
        (processStatement (secondary args) state)
        
        ; Otherwise, the condition fails, so we check if there's an "else" statement, and pass that state if so
        (if (ternary? args)
            ; If else, return the state from that
            (processStatement (ternary args) state)

            ; If no else, do nothing
            state
            )
        )
  )
)


; Takes in the args of a while construct
; Returns the final state when the loop break via accumulator recursion
; (Intermediate recursive steps return the state after one processing of the body statement)
(define processWhile
  (lambda (args state)
    ; Is the while condition true?
    (if (eq? (evaluateCondition (primary args) state) TRUE)
        
        ; If so, mutate the state and recurse
        (processWhile args (processStatement (secondary args) state))
        
        ; Otherwise, return the current state
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
    ; Definition for readability
    (define result (processStatement (currentStatement statementList) state))

    ; Is the result a state?
    (if (state? result)
        ; If it is, we didn't return, so accumulate the state and move forward
        (stateProgress (remainingStatements statementList) result)
        
        ; If it isn't, we returned! Give out the value (this is the base case.)
        result
        )
  )
)

; The main interpret function
; Literally just starts the stateProgress lawnmower with an initial state and statementList
; Takes the filename as input, and gives the return value as outputd
(define interpret
  (lambda (filename)
    (stateProgress (parser filename) voidState)
    )
  )
