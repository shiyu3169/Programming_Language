#lang pl 04

#| BNF for the ALGAE language:
     <ALGAE> ::= <num>
               | <True>
               | <False>
               | { Not <ALGAE> }
               | { And <ALGAE> <ALGAE> } 
               | { Or <ALGAE> <ALGAE> }
               | { if <ALGAE> <ALGAE> <ALGAE> }
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> <ALGAE> ... }
               | { / <ALGAE> <ALGAE> ... }
               | { with { <id> <ALGAE> } <ALGAE> }
               | <id>
               | { < <ALGAE> <ALGAE> }
               | { = <ALGAE> <ALGAE> }
               | { <= <ALGAE> <ALGAE> }
|#

;; ALGAE abstract syntax trees
(define-type ALGAE
  [Num  Number]
  [Bool Boolean]
  [If ALGAE ALGAE ALGAE]
  [Add  (Listof ALGAE)]
  [Mul  (Listof ALGAE)]
  [Sub  ALGAE (Listof ALGAE)]
  [Div  ALGAE (Listof ALGAE)]
  [Id   Symbol]
  [With Symbol ALGAE ALGAE]
  [Less ALGAE ALGAE]
  [Equal ALGAE ALGAE]
  [LessEq ALGAE ALGAE])

(: parse-sexpr : Sexpr -> ALGAE)
;; to convert s-expressions into ALGAEs
(define (parse-sexpr sexpr)
  ;; utility for parsing a list of expressions
  (: parse-sexprs : (Listof Sexpr) -> (Listof ALGAE))
  (define (parse-sexprs sexprs) (map parse-sexpr sexprs))
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) 
     (cond 
       [(equal? name 'True) (Bool true)] 
       [(equal? name 'False) (Bool false)]
       [else (Id name)])]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ (sexpr: args) ...) (Add (parse-sexprs args))]
    [(list '* (sexpr: args) ...) (Mul (parse-sexprs args))]
    [(list '- fst (sexpr: args) ...)
     (Sub (parse-sexpr fst) (parse-sexprs args))]
    [(list '/ fst (sexpr: args) ...)
     (Div (parse-sexpr fst) (parse-sexprs args))]
    [(list '< fst snd) (Less (parse-sexpr fst) (parse-sexpr snd))]
    [(list '= fst snd) (Equal (parse-sexpr fst) (parse-sexpr snd))]
    [(list '<= fst snd) (LessEq (parse-sexpr fst) (parse-sexpr snd))]
    [(list 'if b b1 b2) (If (parse-sexpr b) (parse-sexpr b1) (parse-sexpr b2))]
    [(list 'not arg) (Not (parse-sexpr arg))]
    [(list 'and arg1 arg2) (And (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'or arg1 arg2) (Or (parse-sexpr arg1) (parse-sexpr arg2))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> ALGAE)
;; parses a string containing an ALGAE expression to an ALGAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: Not : ALGAE -> ALGAE)
;; bindings for Not
(define (Not expr)
  (If expr (Bool #f) (Bool #t)))

(: And : ALGAE ALGAE -> ALGAE)
;; bindings for And
(define (And expr1 expr2)
  (If expr1 expr2 (Bool #f)))

(: Or : ALGAE ALGAE -> ALGAE)
;;  bindings for Or
(define (Or expr1 expr2)
  (If expr1 (Bool #t) expr2))

#| Formal specs for `subst':
   (`N' is a <num>, `B' is a True or False `E1', `E2' are <ALGAE>s, 
    `x' is some <id>, `y' is a
   *different* <id>)
      N[v/x]                = N
      B[v/x]                = B
      {+ E ...}[v/x]        = {+ E[v/x] ...}
      {* E ...}[v/x]        = {* E[v/x] ...}
      {- E1 E ...}[v/x]     = {- E1[v/x] E[v/x] ...}
      {/ E1 E ...}[v/x]     = {/ E1[v/x] E[v/x] ...}
      {< E1 E2}[v/x]        = {< E1[v/x] E2[v/x]}
      {= E1 E2}[v/x]        = {< E1[v/x] E2[v/x]}
      {<= E1 E2}[v/x]        = {< E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {if E E1 E2}[v/x]     = {if E[v/x] E1[v/x] E2[v/x]}
|#

(: subst : ALGAE Symbol ALGAE -> ALGAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  ;; convenient helper -- no need to specify `from' and `to'
  (: subst* : ALGAE -> ALGAE)
  (define (subst* x) (subst x from to))
  ;; helper to substitute lists
  (: substs* : (Listof ALGAE) -> (Listof ALGAE))
  (define (substs* exprs) (map subst* exprs))
  (cases expr
    [(Num n)        expr]
    [(Bool b)       expr]
    [(If b b1 b2)   (If (subst* b) (subst* b1) (subst* b2))]
    [(Add args)     (Add (substs* args))]
    [(Mul args)     (Mul (substs* args))]
    [(Sub fst args) (Sub (subst* fst) (substs* args))]
    [(Div fst args) (Div (subst* fst) (substs* args))]
    [(Less fst snd) (Less (subst* fst) (subst* snd))]
    [(Equal fst snd) (Equal (subst* fst) (subst* snd))]
    [(LessEq fst snd) (LessEq (subst* fst) (subst* snd))]
    [(Id name)      (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst* named-expr)
           (if (eq? bound-id from)
             bound-body
             (subst* bound-body)))]))

#| Formal specs for `eval':
     eval(N)            = N
     eval(B)            = B
     eval({+ E ...})    = evalN(E) + ...
     eval({* E ...})    = evalN(E) * ...
     eval({- E})        = -evalN(E)
     eval({/ E})        = 1/evalN(E)
     eval({- E1 E ...}) = evalN(E1) - (evalN(E) + ...)
     eval({/ E1 E ...}) = evalN(E1) / (evalN(E) * ...)
     eval({< E1 E2})    = evalN(E1) < (evalN(E2)
     eval({= E1 E2})    = evalN(E1) = (evalN(E2)
     eval({<= E1 E2})   = evalN(E1) <= (evalN(E2)
     eval(id)           = error! 
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     evalN(E) = eval(E) if it is a number, error otherwise
     evalB(E) = eval(E) if it is a boolean, error otherwise
     eval({if E E1 E2}  = Eval(evalB b) if true, b1, else b2 
     eval({not E})      = eval({if E False True})
     eval({and E1 E2})  = eval({if E1 E2 False})
     eval({or E1 E2})   = eval({if E1 True E2})
|#

(: eval-number : ALGAE -> Number)
;; helper for `eval': verifies that the result is a number
(define (eval-number expr)
  (let ([result (eval expr)])
    (if (number? result)
      result
      (error 'eval-number "need a number when evaluating ~s, but got ~s"
             expr result))))

(: eval-boolean : ALGAE -> Boolean)
;; helper for `eval': verifies that the result is a number
(define (eval-boolean expr)
  (let ([result (eval expr)])
    (if (boolean? result)
      result
      (error 'eval-number "need a boolean when evaluating ~s, but got ~s"
             expr result))))

(: value->algae : (U Number True False) -> ALGAE)
;; converts a value to an ALGAE value (so it can be used with `subst')
(define (value->algae val)
  (cond [(number? val) (Num val)]
        [(boolean? val) (Bool val)]
        ;; Note: since we use Typed Racket, the type checker makes sure
        ;; that this function is never called with something that is not
        ;; in its type, so there's no need for an `else' branch.
        ;; (Strictly speaking, there's no need for the last predicate
        ;; (which is the only one here until you extend this), but it's
        ;; left in for clarity)
        ;; [else (error 'value->algae "unexpected value: ~s" val)]
        ))
;; The following test is also not needed.  In the untyped version, it
;; was needed because the error could not be achieved through `eval' --
;; which is exactly why the above type works.
;; ;; test for an otherwise unreachable error:
;; (test (value->algae null) =error> "unexpected value")

(: eval : ALGAE -> (U Number Boolean))
;; evaluates ALGAE expressions by reducing them to numbers
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Bool b) b]
    [(Add args) (if (null? args) 
                    (error 'eval "no arguments in args")
                    (foldl + 0 (map eval-number args)))]
    [(Mul args) (if (null? args) 
                    (error 'eval "no arguments in args")
                    (foldl * 1 (map eval-number args)))]
    [(Sub fst args) (if (null? args)
                        (- (eval-number fst))
                        (- (eval-number fst) 
                           (foldl + 0 (map eval-number args))))]
    [(Div fst args) (if (null? args)
                        (/ 1 (eval-number fst))
                        (let ([t (foldl * 1 (map eval-number args))])
                          (if (zero? t)
                              (error 'eval "division by zero")
                              (/ (eval-number fst) t))))]
    [(Less fst snd) (< (eval-number fst) (eval-number snd))]
    [(Equal fst snd) (= (eval-number fst) (eval-number snd))]
    [(LessEq fst snd) (<= (eval-number fst) (eval-number snd))]
    [(If b b1 b2) (eval (if (eval-boolean b) b1 b2))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  ;; see the above `value->algae' helper
                  (value->algae (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))

(: run : String -> (U Number Boolean))
;; evaluate an ALGAE program contained in a string
(define (run str)
  (eval (parse str)))


;; tests
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x 5} {+ x x}}") => 10)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)
(test (run "{* 5 5}") => 25) 
(test (run "{/ 5 5}") => 1)
(test (run "{- 5}") => -5) 
(test (run "{/ 5}") => 1/5)
(test (run "{with {x 5} {* x x}}") => 25)
(test (run "{with {x 5} {/ x x}}") => 1)
(test (run "{with anything}")=error> "bad `with' syntax")
(test (run "{+ 5 x}") =error> "free identifier")
(test (run "x")=error> "free identifier") 
(test (run "{+ 5 5 5}") => 15)
(test (run "{+}") =error> "no arguments")
(test (run "{- 2}") => -2)
(test (run "{*}") =error> "no arguments")
(test (run "{/ 2}") => 1/2)
(test (run "{/ 2 3 0}") =error> "division by zero")
(test (run "{/ 6 3 2}") => 1)
(test (run "{* 6 3 2}") => 36)
(test (run "{- 6 3 2}") => 1)
(test (run "{< 6 3}") => false)
(test (run "{= 6 6}") => true)
(test (run "{<= 6 6}") => true)
(test (run "{with {x 5} {< x x}}") => false)
(test (run "{with {x 5} {= x x}}") => true)
(test (run "{with {x 5} {<= x x}}") => true)
(test (run "{<= 6 6 6}") =error> "bad syntax")
(test (run "True") => true)
(test (run "{with {x True} x}") => true)
(test (run "False") => false)
(test (run "{with {x False} x}") => false)
(test (run "{if {= 2 2} True False}") => true)
(test (run "{if {= 2 4} True False}") => false)
(test (run "{if {= 2 2} 2 1}") => 2)
(test (run "{with {x 2} {if {= x 4} True False}}") => false)
(test (run "{if 5 True False}") =error> "need a boolean")
(test (run "{+ 5 True}") =error> "need a number")
(test (run "{not True}") => false)
(test (run "{and True True}") => true)
(test (run "{or True True}") => true)
(test (run "{and False False}") => false)
(test (run "{or False False}") => false)
(test (run "{and True False}") => false)
(test (run "{or False True}") => true)
(test (run "{and {< 1 2} {< 4 3}}") => false)
(test (run "{and {< 1 2} True}") => true)
(test (run "{and True 123}") => 123)
(test (run "{and 123 True}") =error> "need a boolean")
(test (run "{or {< 1 2} {< 3 2}}") => true)
(test (run "{or {< 6 3} {< 3 1}}") => false)
(test (run "{with {x 2} {and {< 1 x} {< 4 3}}}") => false)
(test (run "{not 123}") =error> "need a boolean")


;; number of minutes you spent on your homework.
(define minutes-spent 300)