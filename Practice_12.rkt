;; ** The TOY interpreter

#lang pl 12

;;; ==================================================================
;;; Syntax

#| The BNF:
   <TOY> ::= <num>
           | <id>
           | { bind {{ <id> <TOY> } ... } <TOY> <TOY> ...  }
           | { bindrec {{ <id> <TOY> } ... } <TOY> <TOY> ... }
           | { fun { <id> ... } <TOY> <TOY> ... }
           | { rfun { <id> ... } <TOY> <TOY> ... }
           | { if <TOY> <TOY> <TOY> }
           | { <TOY> <TOY> ... }
           | { set! <id> <TOY> }
|#

;; A matching abstract syntax tree datatype:
(define-type TOY
  [Num  Number]
  [Id   Symbol]
  [Bind (Listof Symbol) (Listof TOY) (Listof TOY)]
  [BindRec (Listof Symbol) (Listof TOY) (Listof TOY)]
  [Fun  (Listof Symbol) (Listof TOY)]
  [RFun  (Listof Symbol) (Listof TOY)]
  [Call TOY (Listof TOY)]
  [If   TOY TOY TOY]
  [Set Symbol TOY])

(: unique-list? : (Listof Any) -> Boolean)
;; Tests whether a list is unique, used to guard Bind and Fun values.
(define (unique-list? xs)
  (or (null? xs)
      (and (not (member (first xs) (rest xs)))
           (unique-list? (rest xs)))))

(: parse-sexpr : Sexpr -> TOY)
;; to convert s-expressions into TOYs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'set! more)
     (match sexpr
       [(list 'set! (symbol: name) body) (Set name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `set!' syntax in ~s" sexpr)])]
    [(cons (and binder (or 'bind 'bindrec)) more)
     (match sexpr
       [(list _ (list (list (symbol: names) (sexpr: nameds)) ...) 
              (sexpr: body0) 
              (sexpr: body) ...)
        (if (unique-list? names) 
            ((if (eq? 'bind binder) Bind BindRec) 
             names 
             (map parse-sexpr nameds) 
             (map parse-sexpr (cons body0 body)))
            (error 'parse-sexpr "`~s' got duplicate names: ~s" binder names))]
       [else (error 'parse-sexpr "bad `~s' syntax in ~s" binder sexpr)])]
    [(cons (and f (or 'fun 'rfun)) more)
     (match sexpr
       [(list _ (list (symbol: names) ...)
              (sexpr: body0) (sexpr: body) ...)
        (if (unique-list? names)
            ((if (eq? 'fun f) Fun RFun)
             names 
             (map parse-sexpr (cons body0 body)))
            (error 'parse-sexpr "`~s' got duplicate names: ~s" f names))]
       [else (error 'parse-sexpr "bad `~s' syntax in ~s" f sexpr)])]
    [(cons 'if more)
     (match sexpr
       [(list 'if cond then else)
        (If (parse-sexpr cond) (parse-sexpr then) (parse-sexpr else))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [(list fun (sexpr: args) ...) ; other lists are applications
     (Call (parse-sexpr fun)
           (map parse-sexpr args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> TOY)
;; Parses a string containing an TOY expression to a TOY AST.
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;;; ==================================================================
;;; Values and environments

(define-type ENV
  [EmptyEnv]
  [FrameEnv FRAME ENV])

(define-type VAL
  [BogusV]
  [RktV  Any]
  [FunV  (Listof Symbol) (Listof TOY) ENV Boolean]
  [PrimV ((Listof VAL) -> VAL)])

;; a single bogus value 
(define the-bogus-value (BogusV))

;; a frame is an association list of names and values.
(define-type FRAME = (Listof (List Symbol (Boxof VAL))))
;;;;recursively extends environment with a new frame.
(: raw-extend : (Listof Symbol) (Listof (Boxof VAL)) ENV -> ENV)

(define (raw-extend names box-values env)
  (if (= (length names) (length box-values))
    (FrameEnv (map (lambda ([name : Symbol] [box-val : (Boxof VAL)])
                     (list name box-val))
                   names box-values)
              env)
    (error 'extend "arity mismatch for names: ~s" names)))

(: extend : (Listof Symbol) (Listof VAL) ENV -> ENV)
;; extends an environment with a new frame.
(define (extend names values env)
  (raw-extend names (map (inst box VAL) values) env))

(: extend-rec : (Listof Symbol) (Listof TOY) ENV -> ENV)
;; extends an environment with a new recursive frame.
(define (extend-rec names exprs env)
  ;; note: no need to check the lengths here, since this is only
  ;; called for `bindrec', and the syntax make it impossible to have
  ;; different lengths
  (let*
      ([boxes (map (lambda (x) (box the-bogus-value)) names)]
       [env-rec (raw-extend names boxes env)])
    (for-each (lambda ([name : Symbol] [named : TOY]) 
                (set-box! (lookup name env-rec) (eval named env-rec))) 
              names 
              exprs)
    env-rec))

(: lookup : Symbol ENV -> (Boxof VAL))
;; lookup a symbol in an environment, frame by frame, return its value
;; or throw an error if it isn't bound
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(FrameEnv frame rest)
     (let ([cell (assq name frame)])
       (if cell
         (second cell)
         (lookup name rest)))]))

(: unwrap-rktv : VAL -> Any)
;; helper for `racket-func->prim-val': unwrap a RktV wrapper in
;; preparation to be sent to the primitive function
(define (unwrap-rktv x)
  (cases x
    [(RktV v) v]
    [else (error 'racket-func "bad input: ~s" x)]))

(: racket-func->prim-val : Function -> (Boxof VAL))
;; converts a racket function to a primitive evaluator function which
;; is a PrimV holding a ((Listof VAL) -> VAL) function.  (the
;; resulting function will use the list function as is, and it is the
;; list function's responsibility to throw an error if it's given a
;; bad number of arguments or bad input types.)
(define (racket-func->prim-val racket-func)
  (define list-func (make-untyped-list-function racket-func))
  (box (PrimV (lambda (args)
                (RktV (list-func (map unwrap-rktv args)))))))

;; The global environment has a few primitives:
(: global-environment : ENV)
(define global-environment
  (FrameEnv (list (list '+ (racket-func->prim-val +))
                  (list '- (racket-func->prim-val -))
                  (list '* (racket-func->prim-val *))
                  (list '/ (racket-func->prim-val /))
                  (list '< (racket-func->prim-val <))
                  (list '> (racket-func->prim-val >))
                  (list '= (racket-func->prim-val =))
                  ;; values
                  (list 'true  (box (RktV #t)))
                  (list 'false (box (RktV #f))))
            (EmptyEnv)))

;;; ==================================================================
;;; Evaluation

(: get-boxes : (Listof TOY) ENV -> (Listof (Boxof VAL)))
;; consumes the expressions and returns a suitable list of boxes.
(define (get-boxes arg-exprs env)
  (map (lambda ([expr : TOY]) 
         (cases expr 
           [(Id name) (lookup name env)]
           [else (error 'get-boxes "non-identifier")])) 
       arg-exprs))


(: eval : TOY ENV -> VAL)
;; evaluates TOY expressions.
(define (eval expr env)
  ;; convenient helper
  (: eval* : TOY -> VAL)
  (define (eval* expr) (eval expr env))
  (cases expr
    [(Num n)   (RktV n)]
    [(Id name) (unbox (lookup name env))]
    [(Bind names exprs bound-body)
     (eval-body bound-body (extend names (map eval* exprs) env))]
    [(BindRec names exprs bound-body)
     (eval-body bound-body (extend-rec names exprs env))]
    [(Fun names bound-body)
     (FunV names bound-body env #f)]
    [(RFun names bound-body)
     (FunV names bound-body env #t)]
    [(Call fun-expr arg-exprs)
     (let ([fval (eval* fun-expr)])
       (cases fval
         [(PrimV proc) (proc (map eval* arg-exprs))]
         [(FunV names body fun-env by-ref?)
          (if by-ref?
              (eval-body body (raw-extend names
                                          (get-boxes arg-exprs env)
                                          fun-env))
              (eval-body body (extend names (map eval* arg-exprs) fun-env)))]
[else (error 'eval "function call with a non-function: ~s"
                      fval)]))]
    [(If cond-expr then-expr else-expr)
     (eval* (if (cases (eval* cond-expr)
                  [(RktV v) v] ; Racket value => use as boolean
                  [else #t])   ; other values are always true
              then-expr
              else-expr))]
    [(Set name body)
     (set-box! (lookup name env) (eval* body)) the-bogus-value]))

(: eval-body : (Listof TOY) ENV -> VAL)
;; evaluates a list of expressions, returns the last value.
(define (eval-body exprs env)
  (foldl (lambda ([x : TOY] [y : VAL]) (eval x env)) the-bogus-value exprs))

(: run : String -> Any)
;; evaluate a TOY program contained in a string
(define (run str)
  (let ([result (eval (parse str) global-environment)])
    (cases result
      [(RktV v) v]
      [else (error 'run
                   "evaluation returned a bad value: ~s" result)])))

;;; ==================================================================
;;; Tests 

(test (run "{{fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}} {add3 1}}")
      => 4)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}
                   {add1 {fun {x} {+ x 1}}}}
              {bind {{x 3}} {add1 {add3 x}}}}")
      => 7)
(test (run "{bind {{identity {fun {x} x}}
                   {foo {fun {x} {+ x 1}}}}
              {{identity foo} 123}}")
      => 124)
(test (run "{bind {{x 3}}
              {bind {{f {fun {y} {+ x y}}}}
                {bind {{x 5}}
                  {f 4}}}}")
      => 7)
(test (run "{{{fun {x} {x 1}}
              {fun {x} {fun {y} {+ x y}}}}
             123}")
      => 124)

;; More tests for complete coverage
(test (run "{bind x 5 x}")      =error> "bad `bind' syntax")
(test (run "{fun x x}")         =error> "bad `fun' syntax")
(test (run "{if x}")            =error> "bad `if' syntax")
(test (run "{}")                =error> "bad syntax")
(test (run "{bind {{x 5} {x 5}} x}") =error> "bind* duplicate names")
(test (run "{fun {x x} x}")     =error> "fun* duplicate names")
(test (run "{+ x 1}")           =error> "no binding for")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{1 2}")             =error> "with a non-function")
(test (run "{{fun {x} x}}")     =error> "arity mismatch")
(test (run "{if {< 4 5} 6 7}")  => 6)
(test (run "{if {< 5 4} 6 7}")  => 7)
(test (run "{if + 6 7}")        => 6)
(test (run "{fun {x} x}")       =error> "returned a bad value")

;;; ==================================================================
;;Even more tests
(test (run "{set! {+ x 1} x}")  =error> "bad `set!' syntax")

(test (run "{bindrec {{fact {fun {n}
                              {if {= 0 n}
                                1
                                {* n {fact {- n 1}}}}}}}
              {fact 5}}")
      => 120)
(test (run "{bind {{x 1}} {bind {{y {set! x 2}}} x}}") => 2)

(test (run "{bind {{make-counter
                     {fun {}
                       {bind {{c 0}}
                         {fun {}
                           {set! c {+ 1 c}}
                           c}}}}}
              {bind {{c1 {make-counter}}
                     {c2 {make-counter}}}
                {* {c1} {c1} {c2} {c1}}}}")
      => 6)
(test (run "{bindrec {{foo {fun {}
                             {set! foo {fun {} 2}}
                             1}}}
              {+ {foo} {* 10 {foo}}}}")
      => 21)

(test (run "{{rfun {x} x} {/ 4 0}}") =error> "non-identifier")
(test (run "{5 {/ 6 0}}") =error> "non-function")
(test (run "{bind {{swap! {rfun {x y}
                            {bind {{tmp x}}
                              {set! x y}
                              {set! y tmp}}}}
                   {a 1}
                   {b 2}}
              {swap! a b}
              {+ a {* 10 b}}}")
      => 12)

(define minutes-spent 600)