#lang racket
(require "helpers.rkt")
(require "state.rkt")
(require "operators.rkt")
(require "evaluators.rkt")
(require "classParser.rkt")
(provide (all-defined-out))

; ========================
; GROUP NAMES AND THOUGHTS
; ========================
#|
The code below is the product of Devin Riehle, built open the work down by himself and others in previous parts.


Unfortunately, within the time frame I found myself in I couldn't get polymorphism working :(
I know conceptually how it's meant to work, theoretically one follows Java's example by first
making an implicit call to the "super" constructor, then for each declaration afterwards you ignore redeclaration
warnings. (I'd probably do this by technically making them new state layers, then merging them all at the end)

Then you end up with, as in test 7, some closure like (C 4 222 3 22 2 1) for fields (w y)
Then looking for w or y works as normal for c, and when calling a super, you change the callingInstance to be
(C 3 22 2 1) for fields (z y) by removing the length of C's fields. This happens again for the 2nd super call,
and thus (C 2 1) for fields (y x).

HOWEVER, "this" has remained as the original closure in the state for this whole climb, so this.m2()
resolves to (C 4 222 3 22 2 1).m2(), which climbs up a super branch once m2 isn't found in C to
(C 3 22 2 1).m2() for fields (z y), and m2 is x + y + z
z and y are trivialy looked up, but when we can't find x in the fields, the lookup function cuts
the length of it's fields from the front of its valueList and fetches its parent's fields, then finding x as normal
and arriving at 26 as ordained.

Although I didn't quite manage to complete this within the requisite amount of time, this has been a very
interesting project academics wise, and I'll probably continue tweaking it for a while as a curiousity.

With gratitude,
Devin
|#

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
    
    (classClosure superClass (ternary classArgs) (primary classArgs))
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
    (define mainClosure (methodFromClass 'main className classState))
    ;(list mainClosure null classState echoDouble (lambda (finalValue finalState) finalValue) null)
    (callFunction mainClosure null classState echoDouble (lambda (finalValue finalState) finalValue) null)
    )
  )
