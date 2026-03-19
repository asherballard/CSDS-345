#lang racket
(require "helpers.rkt")
(require "state.rkt")
(require "operators.rkt")
(require "evaluators.rkt")
(require "simpleParser.rkt")
(provide (all-defined-out))

; ============================
; INTERPRETER HELPER FUNCTIONS
; ============================
; Functions only related to the interpreter that do intermediate steps

; Helper function that turns a statement into a state mapping for getting "next"
(define getStateMapping
  (lambda (statement state)
    (define args (argList statement))
    (define op (operator statement))
    (cond
      ; If the statement is a declaration, check if its a redeclaration, then parse as appropriate
      [(eq? op 'var) (if (isDeclared? (primary args) state)
                         (error "Error: variables cannot be re-declared")
                         (if (secondary? args)
                             (lambda (inputState) (assign (primary args) (evaluateExpression (secondary args) inputState) inputState))
                             (lambda (inputState) (declare (primary args) inputState))
                             )
                         )]
      ; If the statement is an assignment, check to make sure var is declared
      [(eq? op '=) (if (isDeclared? (primary args) state)
                       (lambda (inputState) (assign (primary args) (evaluateExpression (secondary args) inputState) inputState))
                       (error "Error: attemped to assign to undeclared variable")
                       )]
      )
    )
  )


; =======================
; STATE MAPPING FUNCTIONS
; =======================
; Each of these functions take in constructs and the current state
; And returns a new state
; I.e. M_state mappings

; Takes a statementList, line of code to be executed, and other continuations
; Continues until it hits a return
(define nextState
  (lambda (state statementList next break continue return throw)
    
    (define statement (if (null? statementList) null (currentStatement statementList)))
    (define op (operator statement))
    (define args (argList statement))
    (define newState (next state))
    (define tail (remainingStatements statementList))
    
    (cond
      ; Check for end of block
      [(null? statement) newState]
      
      ; If we're returning, do that
      [(eq? 'return op) (return (evaluateExpression (primary (argList statement)) newState) newState)]
      
      ; If we're assigning or declaring, pass that into next
      [(or (eq? 'var op) (eq? '= op)) (nextState newState tail (getStateMapping statement newState) break continue return throw)]

      ; If it's an if statement, evaluate the condition and apply next appropriately
      [(eq? op 'if) (if (eq? (evaluateCondition (primary args) newState) TRUE)
                        ; True condition means we put the first statement on the statementList
                        (nextState newState (cons (secondary args) tail) echo break continue return throw)

                        ; Check for an else condition, put it on the list if exists
                        (if (ternary? args)
                            (nextState newState (cons (ternary args) tail) echo break continue return throw)
                            (nextState newState tail echo break continue return throw)
                            )
                        )]

      ; If it's a while statement, keep reprocessing the statement until the condition is false or we break
      [(eq? op 'while) (if (eq? (evaluateCondition (primary args) newState) TRUE)
                           ; Put the true statement in front of while so it executes before checking again
                           (nextState newState (cons (secondary args) statementList) echo break continue return throw)

                           ; If condition is false, do nothing
                           (nextState newState tail echo break continue return throw)
                           )]
      [else (error "Unrecognized operator when progressing")]
      )
    )
  )

; ===========
; SCOPE TOOLS
; ===========



; ==================
; MAIN FUNCTION
; ==================

; The main interpret function
; Literally just starts the stateProgress lawnmower with an initial state and statementList
; Takes the filename as input, and gives the return value as outputd
(define interpret
  (lambda (filename)
    (nextState voidState (parser filename)
               ; Next
               echo
               ; Break
               echo
               ; Continue
               echo
               ; Return
               (lambda (value state) (echo value))
               ; Throw
               (lambda (value state) (makePairedList value state))
               )
    )
  )
