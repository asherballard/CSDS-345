#lang racket
(require "helpers.rkt")
(require "state.rkt")
(require "operators.rkt")
(require "evaluators.rkt")
(require "functionParser.rkt")
(provide (all-defined-out))

; =====================
; INTERPRETER FUNCTIONS
; =====================

; The initial state generator, which grabs functions and does variable assignments before evaluating main()
(define processOuterLayer
  (lambda (statementList state)
    
    ; Get statement info once
    (define statement (if (null? statementList) null (currentStatement statementList)))
    (define tail (if (null? statementList) null (remainingStatements statementList)))
    (define op (if (null? statement) null (operator statement)))
    
    (cond
      ; If we've reach the end of the outer layer, begin interpreting main
      [(null? statement) (runMain state)]
      
      ; If function, add its closure to the state
      [(eq? op 'function) (if (isLive? (primary (argList statement)) state)
                         (error "Function already declared in this (global) scope")
                         (processOuterLayer tail (funcDeclare (argList statement) state))
       )]

      ; If assignment, add it to the state
      [(eq? 'var op) (if (isLive? (primary (argList statement)) state)
                         (error "Variable already live")
                         (if (secondary? (argList statement))
                             (evaluateExpression (secondary (argList statement)) state echoDouble
                                                 (lambda (val retState)
                                                   (define nState (declareAssign (primary (argList statement)) val retState))
                                                   (processOuterLayer tail nState)
                                                   )
                                                 )
                             (processOuterLayer tail (declare (primary (argList statement))))
                             )
       )]

      ; I don't think this is allowed by the parser, but it's harmless to leave here just in case.
      [(eq? '= op) (if (isDeclared? (primary (argList statement)) state)
                       (evaluateExpression (secondary (argList statement)) state echoDouble (lambda (val retState)
                                                                                              (define nState (assign (primary (argList statement)) val retState))
                                                                                              (processOuterLayer tail nState)
                                                                             ))
                       (error "Variable undeclared")
                       )]

      [else (error "How did we get here?")]
      )
    )
  )


; Evaluates the main() function with a given initial state
(define runMain
  (lambda (state)
    (callFunction 'main null state echoDouble (lambda (finalValue finalState) finalValue))
    )
  )


; The main interpret function
; Calls processOuterLayer
; Takes the filename as input, and gives the return value as outputd
(define interpret
  (lambda (filename)
    (processOuterLayer (parser filename) voidState)
    )
  )
