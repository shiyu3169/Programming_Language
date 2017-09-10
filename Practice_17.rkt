#lang pl 17

(define-type Token = (U Symbol Integer))

;; A macro that defines a DFA language
(define-syntax automaton
  (syntax-rules (: ->)
    [(automaton init-state end-state
                [state : (input-sym -> new-state) ...]
                ...)
     (lambda (string)
       (: state : (Listof Token) -> Boolean)
       ...
       (define (state stream)
         (match stream
           ['() (if (symbol=? 'state 'end-state) #t #f)]
           [(cons 'input-sym more) (new-state more)]
           ...
           [_ #f]))
       ...
       (init-state (explode-string string)))]))

(: cXr : String -> Boolean)
;; Identifies strings that match "c[ad]*r+"
(define cXr (automaton init end ; is the accepting state
                       [init : (c -> more)]
                       [more : (a -> more)
                             (d -> more)
                             (r -> end)]
                       [end  : (r -> end)]))
;; tests:
(test (cXr "cadr"))
(test (cXr "cadadadadadadddddaaarrr"))
(test (not (cXr "ccadr")))
(test (not (cXr "c"))) ; BAD TEST!

(: div5 : String -> Boolean)
;; Determine whether a binary number is divisible by 5
(define div5
  (automaton mod0 mod0
             [mod0 : (0 -> mod0) (1 -> mod1)]
             [mod1 : (0 -> mod2) (1 -> mod3)]
             [mod2 : (0 -> mod4) (1 -> mod0)]
             [mod3 : (0 -> mod1) (1 -> mod2)]
             [mod4 : (0 -> mod3) (1 -> mod4)]))
;; tests:
(test (div5 ""))
(test (div5 "0"))
(test (div5 "000"))
(test (div5 (number->string 12345 2)))
(test (not (div5 (number->string 123453 2))))
(test (not (div5 "1001")))

;; A macro that defines a PDA language
(define-syntax pushdown
  (syntax-rules (: ->)
    [(pushdown init-state end-state
               [state : ((input-stream ...)
                         (input-stack ...)
                         ->
                         new-state
                         (new-stack ...)) ...]
               ...)
     (lambda (string)
       (: state : (Listof Token) (Listof Token) -> Boolean)
       ...
       (define (state stream stack)
         (match (list stream stack)
           [(list '() '()) (if (symbol=? 'state 'end-state) #t #f)]
           [(list (list-rest 'input-stream ... more-input)
                  (list-rest 'input-stack ... more-stack))
            (new-state more-input (list* 'new-stack ... more-stack))]
           ...
           [_ #f]))
       ...
       (init-state (append (explode-string string) '(*)) '(*)))]))

(: balanced : String -> Boolean)
;; Identifies strings that contain only balanced parentheses
(define balanced (pushdown init init
                           [init : ((open) ()      -> init (open))
                                 ((close) (open) -> init ())
                                 ((*) (*)        -> init ())]))
;; tests:
(test (balanced ""))
(test (balanced "()"))
(test (balanced "(((())))"))
(test (balanced "((()())(()))"))
(test (not (balanced "(")))
(test (not (balanced ")")))
(test (not (balanced ")(")))

(: zeros=ones : String -> Boolean)
;; Identifies strings of n 0s followed by n 1s
(define zeros=ones
  (pushdown 0s end
            [0s  : ((0) ()  -> 0s  (A))
                 (()  ()  -> 1s  ())]
            [1s  : ((1) (A) -> 1s  ())
                 ((*) (*) -> end (*))]
            [end : (()  (*) -> end ())]))
;; tests:
(test (zeros=ones ""))
(test (zeros=ones "01"))
(test (zeros=ones "000111"))
(test (not (zeros=ones "0")))
(test (not (zeros=ones "11")))
(test (not (zeros=ones "10")))
(test (not (zeros=ones "00011")))
(test (not (zeros=ones "00101111")))

(: foo : String -> Boolean)
;; A PDA for testing ending in a non-end state.
(define foo
  (pushdown init end
            [init : ((0) () -> mid ())
                  ((1) () -> end ())]
            [mid  : ((*) (*) -> mid ())]
            [end  : ((*) (*) -> end ())]))

(test (foo "1"))
(test (not (foo "0")))

(define minutes-spent 240)
