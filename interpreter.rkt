#lang racket
(require "helpers.rkt")
(require "state.rkt")
(require "operators.rkt")
(require "evaluators.rkt")
(require "classParser.rkt")
(provide (all-defined-out))

; =====================
; INTERPRETER FUNCTIONS
; =====================

; Turns the arguments of "class" into a closure
(define getClassClosure
  (lambda (classArgs)
    ; If class does not extend anything, we use "Object" as Java does. This is arbitrary.
    (define superClass (if (null? (secondary classArgs))
                           'Object
                           (primary (argList (secondary classArgs)))
                           ))
    
    (classClosure superClass (ternary classArgs))
    )
  )

; Reads a classList into a given state
(define getClassState (lambda (classList) (getClassListInternal classList voidState)))
(define getClassListInternal
  (lambda (classList state)
    (define finished (null? classList))
    (define classStatement (if finished null (currentStatement classList)))
    (define classArgs (if finished null (argList classStatement)))
    (define className (if finished null (primary classArgs)))
    
    (if finished
        state
        (getClassListInternal (remainingStatements classList) (declareAssign className (getClassClosure classArgs) state))
        )
    ))

; The OLD initial state generator, which grabs functions and does variable assignments before evaluating main()
#|(define processOuterLayerOld
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
  )|#


; Evaluates the main() function of className with a given initial state
(define runMain
  (lambda (state className)
    (callFunction 'main null state echoDouble (lambda (finalValue finalState) finalValue))
    )
  )


; The main interpret function
; Calls processOuterLayer
; Takes the filename as input, and gives the return value as outputd
(define interpret
  (lambda (fileName className)
    (define classState (getClassState (parser fileName)))
    (define targetClass (lookupBinding className classState))
    (define mainClosure (lookupBinding 'main (getClassStaticState targetClass)))
    (callFunction mainClosure null classState echoDouble (lambda (finalValue finalState) finalValue))
    )
  )
