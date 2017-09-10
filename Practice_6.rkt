#lang pl 06

#|
The grammar:
  <BRANG> ::= <num>
            | { + <BRANG> <BRANG> }
            | { - <BRANG> <BRANG> }
            | { * <BRANG> <BRANG> }
            | { / <BRANG> <BRANG> }
            | { with { <id> <BRANG> } <BRANG> }
            | <id>
            | { fun { <id> <id> ... } <BRANG> }
            | { call <BRANG> <BRANG> <BRANG> ... }

Evaluation rules:
  eval(N,env)                = N
  eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
  eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
  eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
  eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
  eval(CRef(N),env)          = list-ref(env,N)
  eval({fun {x} E},env)      = <{fun {x} E}, env>
  eval({call E1 E2},env1)
           = eval(Ef, cons(eval(E2,env1),env2))
                             if eval(E1,env1) = <{fun {x} Ef}, env2>
           = error!          otherwise
|#

(define-type BRANG
  [Num  Number]
  [Add  BRANG BRANG]
  [Sub  BRANG BRANG]
  [Mul  BRANG BRANG]
  [Div  BRANG BRANG]
  [Id   Symbol]
  [With Symbol BRANG BRANG]
  [Fun  Symbol (Listof Symbol) BRANG]
  [Call BRANG BRANG (Listof BRANG)])

(define-type CORE
  [CNum  Number]
  [CAdd  CORE CORE]
  [CSub  CORE CORE]
  [CMul  CORE CORE]
  [CDiv  CORE CORE]
  [CRef  Natural]
  [CFun  CORE]
  [CCall CORE CORE])

(: parse-sexpr : Sexpr -> BRANG)
;; to convert s-expressions into BRANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name) (symbol: name2) ... ) body)
        (Fun name name2 (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg args ...) 
     (Call (parse-sexpr fun) (parse-sexpr arg) (map parse-sexpr args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> BRANG)
;; parses a string containing a BRANG expression to a BRANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function
(define-type VAL
  [NumV Number]
  [FunV CORE ENV])

(define-type ENV = (Listof VAL))

(define-type DE-ENV = Symbol -> Natural)

(: de-empty-env : Symbol -> Natural)
;; vacuous mapping — it always throws an error.
(define (de-empty-env s)
  (error 'de-empty-env "empty environment"))

(:  de-extend : DE-ENV Symbol  -> DE-ENV)
;;consumes a DE-ENV and a symbol, and returns the extended environment
(define (de-extend env id)
  (λ ([x : Symbol])
    (if (equal? x id) 0 (+ 1 (env x)))))

(: currify : BRANG -> BRANG)
;;curry the BRANG
(define (currify b)
  (cases b
    [(Fun bound-id bound-ids bound-body)
     (Fun bound-id
          null
          (foldr (lambda ([id : Symbol] [brang : BRANG]) (Fun id null brang))
                 bound-body
                 bound-ids))]
    [(Call fun-expr arg-expr args-expr)
     (foldl (lambda ([arg : BRANG] [brang : BRANG]) (Call brang arg null))
            (Call fun-expr arg-expr null)
            args-expr)]
    [else b]))

(: preprocess : BRANG DE-ENV -> CORE)
;;a simple recursive scan of its input that translates 
;;a given BRANG value (and a DE-ENV mapping) to the corresponding CORE value
(define (preprocess expr env)
  (cases (currify expr)
    [(Num n) (CNum n)]
    [(Add l r) (CAdd (preprocess l env) (preprocess r env))]
    [(Sub l r) (CSub (preprocess l env) (preprocess r env))]
    [(Mul l r) (CMul (preprocess l env) (preprocess r env))]
    [(Div l r) (CDiv (preprocess l env) (preprocess r env))]
    [(With bound-id named-expr bound-body)
     (CCall (CFun (preprocess bound-body (de-extend env bound-id))) 
            (preprocess named-expr env))]
    [(Id n) (CRef (env n))]
    [(Fun bound-id bound-ids bound-body) 
     (CFun (preprocess bound-body (de-extend env bound-id)))]
    [(Call fun-expr arg-expr  args-expr) 
     (CCall (preprocess fun-expr env) (preprocess arg-expr env))]))

(: NumV->number : VAL -> Number)
;; convert a BRANG runtime numeric value to a Racket one
(define (NumV->number v)
  (cases v
    [(NumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" v)]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : CORE ENV -> VAL)
;; evaluates CORE expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(CNum n) (NumV n)]
    [(CAdd l r) (arith-op + (eval l env) (eval r env))]
    [(CSub l r) (arith-op - (eval l env) (eval r env))]
    [(CMul l r) (arith-op * (eval l env) (eval r env))]
    [(CDiv l r) (arith-op / (eval l env) (eval r env))]
    [(CRef n) (list-ref env n)]
    [(CFun bound-body)
     (FunV bound-body env)]
    [(CCall fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-body f-env)
          (eval bound-body
                (cons (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]))

(: run : String -> Number)
;; evaluate a BRANG program contained in a string
(define (run str)
  (let ([result (eval (preprocess (parse str) de-empty-env) null)])
    (cases result
      [(NumV n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)])))

;;tests
(test (run "{+ 1 1}")
      => 2)

(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)

(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)

(test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
      => 7)

(test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
      => 124)

(test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
      => 7)

(test (run "{call {with {x 3}
                    {fun {y} {+ x y}}}
                  4}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
      => 124)

(test (run "{call {fun {x} {- x 1}} 4}")
      => 3)

(test (run "{call {fun {x} {* x 1}} 4}")
      => 4)

(test (run "{call {fun {x} {/ x 1}} 4}")
      => 4)

(test (run "{call {fuc {x} {/ x 1}} 4}")
      =error> "bad syntax")

(test (run "{call 2 4}")
      =error> "expects a function")

(test (run "{with {dasdasadd3 {fun {x} {+ x 3}}}
              {call add3 1}}") =error> "empty environment")


(test (run "{* {fun {x} x} 1}")
      =error> "expected a number")

(test (run "{with 1
              {call add3 1}}")
      =error> "bad `with' syntax")

(test (run "{call {fun {1} {+ x 1}} 4}")
      =error> "bad `fun' syntax")

(test (run "{fun {x} x}")
      =error> "evaluation returned a non-number")

(test (run "{call {fun {x y z} {- x {+ y z}}} 10 5 1}") => 4)
(test (run "{call {fun {x y z} {- x {+ y z}}} 10 5}") =error> "non-number")
(test (run "{call {fun {x y z} {- x y}} 10 5 1}") => 5)

(define minutes-spent 900)