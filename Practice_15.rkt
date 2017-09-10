#lang pl 15

;;; ==================================================================
;;; Syntax

#| The BNF:
   <SLUG> ::= <num>
            | <str>
            | <id>
            | { bind {{ <id> <SLUG> } ... } <SLUG> }
            | { with-stx {<id> { <id> ... }
                               { <pattern> <pattern> } ...}
                  <SLUG> }
            | { fun { <id> ... } <SLUG> }
            | { if <SLUG> <SLUG> <SLUG> }
            | { <SLUG> <SLUG> ... }
|#

;; A matching abstract syntax tree datatype:
;;   (note: no WithStx constructor -- it is preprocessed away)
(define-type SLUG
  [Num  Number]
  [Str  String]
  [Id   Symbol]
  [Bind (Listof Symbol) (Listof SLUG) SLUG]
  [Fun  (Listof Symbol) SLUG]
  [Call SLUG (Listof SLUG)]
  [If   SLUG SLUG SLUG])

(: unique-list? : (Listof Any) -> Boolean)
;; Tests whether a list is unique, guards Bind and Fun values.
(define (unique-list? xs)
  (or (null? xs)
      (and (not (member (first xs) (rest xs)))
           (unique-list? (rest xs)))))

;; This built-in is used in the following code:
;; make-transformer : (Listof Symbol) (Listof (List Sexpr Sexpr))
;;                    -> (Sexpr -> Sexpr)
;; consumes a list of pattern pairs, and creates a transformer
;; function (transforms an s-expression into an s-expression)

(: parse-sexpr : Sexpr (Listof (List Symbol (Sexpr -> Sexpr)))
                 -> SLUG)
;; parses *and* macro-expands s-expressions; the second argument is
;; the association list of transformers at this point.
(define (parse-sexpr sexpr transformers)
  ;; convenient function for common cases where we recurse with the
  ;; same transformers
  (: parse* : Sexpr -> SLUG)
  (define (parse* sexpr) (parse-sexpr sexpr transformers))
  (let ([transformer (and (pair? sexpr)
                          (assq (car sexpr) transformers))])
    (if transformer
      ;; if there is a transformer by this name, apply it and
      ;; continue with the result
      (parse* ((second transformer) sexpr))
      (match sexpr
        ;; if we see `with-stx', then recursively parse with the
        ;; additional transformer that we make
        [(cons 'with-stx more)
         (match sexpr
           [(list 'with-stx
                  (list (symbol: name)
                        (list (symbol: keywords) ...)
                        (list (sexpr: pattern) (sexpr: result)) ...)
                  body)
            (parse-sexpr
             body
             (cons (list name (make-transformer
                               keywords
                               (map (lambda ([p : Sexpr] [r : Sexpr])
                                      (list p r))
                                    pattern result)))
                   transformers))]
           [else (error 'parse-sexpr "bad `with-stx' syntax in ~s"
                        sexpr)])]
        [(number: n)    (Num n)]
        [(symbol: name) (Id name)]
        [(string: s)    (Str s)]
        [(cons 'bind more)
         (match sexpr
           [(list 'bind (list (list (symbol: names) (sexpr: nameds))
                              ...)
              body)
            (if (unique-list? names)
              (Bind names (map parse* nameds) (parse* body))
              (error 'parse-sexpr "duplicate `bind' names: ~s" names))]
           [else (error 'parse-sexpr "bad `bind' syntax in ~s" sexpr)])]
        [(cons 'fun more)
         (match sexpr
           [(list 'fun (list (symbol: names) ...) body)
            (if (unique-list? names)
              (Fun names (parse* body))
              (error 'parse-sexpr "duplicate `fun' names: ~s" names))]
           [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
        [(cons 'if more)
         (match sexpr
           [(list 'if cond then else)
            (If (parse* cond) (parse* then) (parse* else))]
           [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
        [(list fun (sexpr: args) ...) ; other lists are applications
         (Call (parse* fun) (map parse* args))]
        [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))))

(: parse : String -> SLUG)
;; Parses a string containing an SLUG expression to a SLUG AST.
(define (parse str)
  (parse-sexpr (string->sexpr str) null))

;;; ==================================================================
;;; Values and environments

(define-type ENV
  [EmptyEnv]
  [FrameEnv FRAME ENV])

(define-type VAL
  [RktV  Any]
  [FunV  (Listof Symbol) SLUG ENV]
  [ExprV SLUG ENV (Boxof (U #f VAL))]
  [PrimV ((Listof VAL) -> VAL)])

;; I/O descriptions
;; these will hold VALs which should evaluate to the commented types;
;; Slug is not statically typed, so we can't do better than throw
;; run-time errors if wrong values turn out
(define-type IO
  [Print    VAL]      ; String
  [ReadLine VAL]      ; receiver: String -> IO
  [Begin2   VAL VAL]  ; IO IO
  ;; mutation
  [NewRef   VAL VAL]  ; init, receiver for new ref
  [UnRef    VAL VAL]  ; ref, receiver for its value
  [SetRef   VAL VAL]) ; ref, new value

;; a frame is an association list of names and values.
(define-type FRAME = (Listof (List Symbol VAL)))

(: extend : (Listof Symbol) (Listof VAL) ENV -> ENV)
;; extends an environment with a new frame.
(define (extend names values env)
  (if (= (length names) (length values))
    (FrameEnv (map (lambda ([name : Symbol] [val : VAL])
                     (list name val))
                   names values)
              env)
    (error 'extend "arity mismatch for names: ~s" names)))

(: lookup : Symbol ENV -> VAL)
;; looks for a name in an environment, searching through each frame.
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(FrameEnv frame rest)
     (let ([cell (assq name frame)])
       (if cell
         (second cell)
         (lookup name rest)))]))

(: unwrap-rktv : VAL -> Any)
;; helper for `racket-func->prim-val': strict and unwrap a RktV
;; wrapper in preparation to be sent to the primitive function
(define (unwrap-rktv x)
  (let ([s (strict x)])
    (cases s
      [(RktV v) v]
      [else (error 'racket-func "bad input: ~s" s)])))

(: wrap-in-val : Any -> VAL)
;; helper that ensures a VAL output using RktV wrapper when needed,
;; but leaving as is otherwise
(define (wrap-in-val x)
  (if (VAL? x) x (RktV x)))

(: racket-func->prim-val : Function Boolean -> VAL)
;; converts a racket function to a primitive evaluator function which
;; is a PrimV holding a ((Listof VAL) -> VAL) function.  (the
;; resulting function will use the list function as is, and it is the
;; list function's responsibility to throw an error if it's given a
;; bad number of arguments or bad input types.)
(define (racket-func->prim-val racket-func strict?)
  (define list-func (make-untyped-list-function racket-func))
  (PrimV (lambda ([args : (Listof VAL)])
           (let ([args (if strict? (map unwrap-rktv args) args)])
             (wrap-in-val (list-func args))))))

;; The global environment has a few primitives:
(: global-environment : ENV)
(define global-environment
  (FrameEnv (list (list '+  (racket-func->prim-val +  #t))
                  (list '-  (racket-func->prim-val -  #t))
                  (list '*  (racket-func->prim-val *  #t))
                  (list '/  (racket-func->prim-val /  #t))
                  (list '<  (racket-func->prim-val <  #t))
                  (list '>  (racket-func->prim-val >  #t))
                  (list '=  (racket-func->prim-val =  #t))
                  (list '<= (racket-func->prim-val <= #t))
                  (list '>= (racket-func->prim-val >= #t))
                  (list 'number->string
                        (racket-func->prim-val number->string #t))
                  (list 'string->number
                        (racket-func->prim-val string->number #t))
                  ;; note flags:
                  (list 'cons  (racket-func->prim-val cons  #f))
                  (list 'list  (racket-func->prim-val list  #f))
                  (list 'first (racket-func->prim-val first #t))
                  (list 'rest  (racket-func->prim-val rest  #t))
                  (list 'null? (racket-func->prim-val null? #t))
                  ;; IO constructors -- all are non-strict
                  (list 'print  (racket-func->prim-val Print    #f))
                  (list 'read   (racket-func->prim-val ReadLine #f))
                  (list 'begin2 (racket-func->prim-val Begin2   #f))
                  (list 'newref   (racket-func->prim-val NewRef   #f))
                  (list 'unref    (racket-func->prim-val UnRef    #f))
                  (list 'set-ref! (racket-func->prim-val SetRef   #f))
                  ;; values
                  (list 'true  (RktV #t))
                  (list 'false (RktV #f))
                  (list 'null  (RktV null)))
            (EmptyEnv)))

;;; ==================================================================
;;; Evaluation

(: eval-promise : SLUG ENV -> VAL)
;; used instead of `eval' to create an evaluation promise
(define (eval-promise expr env)
  (ExprV expr env (box (ann #f : (U #f VAL)))))

(: strict : VAL -> VAL)
;; forces a (possibly nested) ExprV promise, returns a VAL that is not
;; an ExprV
(define (strict v)
  (cases v
    [(ExprV expr env cache)
     (or (unbox cache)
         (let ([val (strict (eval expr env))])
           (set-box! cache val)
           val))]
    [else v]))

(: eval : SLUG ENV -> VAL)
;; evaluates SLUG expressions.
(define (eval expr env)
  ;; convenient helper
  (: eval* : SLUG -> VAL)
  (define (eval* expr) (eval-promise expr env))
  (cases expr
    [(Num n)   (RktV n)]
    [(Str s)   (RktV s)]
    [(Id name) (lookup name env)]
    [(Bind names exprs bound-body)
     (eval bound-body (extend names (map eval* exprs) env))]
    [(Fun names bound-body)
     (FunV names bound-body env)]
    [(Call fun-expr arg-exprs)
     (let ([fval (strict (eval* fun-expr))]
           [arg-vals (map eval* arg-exprs)])
       (cases fval
         [(PrimV proc) (proc arg-vals)]
         [(FunV names body fun-env)
          (eval body (extend names arg-vals fun-env))]
         [else (error 'eval "function call with a non-function: ~s"
                      fval)]))]
    [(If cond-expr then-expr else-expr)
     (eval* (if (cases (strict (eval* cond-expr))
                  [(RktV v) v] ; Racket value => use as boolean
                  [else #t])   ; other values are always true
              then-expr
              else-expr))]))

(: run : String -> Any)
;; evaluate a SLUG program contained in a string
(define (run str)
  (let ([result (strict (eval (parse str) global-environment))])
    (cases result
      [(RktV v) v]
      [else (error 'run "evaluation returned a bad value: ~s"
                   result)])))

;;; ==================================================================
;;; Refs -- similar to a box, but always holds an `Any' value

;; This should be considered an internal definition, use only the
;; following functions
(define-type REF [Ref (Boxof Any)])

;; These are the public interface functions, use them similarly to how
;; you'd use box functions
(: ref : Any -> REF)
(define (ref x) (Ref (box x)))
(: unref : REF -> Any)
(define (unref r) (cases r [(Ref b) (unbox b)]))
(: set-ref! : REF Any -> Void)
(define (set-ref! r x) (cases r [(Ref b) (set-box! b x)]))
;; you'll also need a predicate -- instead of using `REF?', define a
;; public `ref?' (it will get the same predicate type, which has a
;; special meaning in typed racket):
(define ref? REF?)

;;; ==================================================================
;;; I/O execution

(: execute-print : VAL -> Void)
;; executes a `print' description
(define (execute-print val)
  (let ([str (cases val [(RktV x) (and (string? x) x)] [else #f])])
    (if str
        (printf "~a" str)
        (error 'execute-print "cannot `print' a non-string value: ~s" val))))

(: execute-begin2 : VAL VAL -> Void)
;; executes a `begin2' description
(define (execute-begin2 1st 2nd)
  (execute-val 1st)
  (execute-val 2nd))

(: execute-receiver : VAL (-> Any) -> Void)
;; helper for executing receivers, wraps the value in a RktV, and
;; calls the receiver with the value, if it is valid.  Note that the
;; value is actually a `producer' thunk, because it might involve some
;; side-effect (like reading) that we want to avoid if the receiver is
;; invalid.
(define (execute-receiver receiver producer)
  (cases receiver
    [(FunV names body env)
     (execute-val 
      (eval body (extend names (list (wrap-in-val (producer))) env)))]
    [else (error 'execute-receiver "expecting a receiver function")]))

(: execute-read : VAL -> Void)
;; executes a `read' description
(define (execute-read receiver)
  (execute-receiver receiver read-line))

(: execute-newref : VAL VAL -> Void)
;; executes a new-ref
(define (execute-newref init receiver)
  (execute-receiver receiver (lambda () (ref init))))

(: execute-unref : VAL VAL -> Void)
;; executes an un-ref
(define (execute-unref pre-ref receiver)
  (let ([pre-ref-typed (cases pre-ref [(RktV x) (and (ref? x) x)] [else #f])])
    (if pre-ref-typed
        (execute-receiver receiver (lambda () (unref pre-ref-typed)))
        (error 'execute-unref "expected a Ref: ~s" pre-ref))))

(: execute-setref : VAL VAL -> Void)
;; executes a set-ref
(define (execute-setref new-ref new-value)
  (let ([new-ref-typed (cases new-ref [(RktV x) (and (ref? x) x)] [else #f])])
    (if new-ref-typed
        (set-ref! new-ref-typed new-value)
        (error 'execute-unref "expected a Ref: ~s" new-ref))))

(: execute-val : VAL -> Void)
;; extracts an IO from a VAL and executes it
(define (execute-val val)
  (let* ([val (strict val)]
         [io  (cases val
                [(RktV x) (and (IO? x) x)]
                [else #f])])
    (if (not io)
      (error 'execute-val "expecting an IO value: ~s" val)
      (cases io
        [(Print x)    (execute-print (strict x))]
        [(ReadLine x) (execute-read (strict x))]
        [(Begin2 x y) (execute-begin2 x y)]
        [(NewRef x y) (execute-newref x (strict y))]
        [(UnRef x y)  (execute-unref (strict x) (strict y))]
        [(SetRef x y) (execute-setref (strict x) y)]))))

(: run-io : String -> Void)
;; evaluate a SLUG program contained in a string, and execute the
;; resulting IOV description
(define (run-io str)
  (execute-val (eval (parse str) global-environment)))

(define minutes-spent 240)

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
(test (run "{bind {{x 5} {x 5}} x}") =error> "duplicate*bind*names")
(test (run "{fun {x x} x}")     =error> "duplicate*fun*names")
(test (run "{+ x 1}")           =error> "no binding for")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{1 2}")             =error> "with a non-function")
(test (run "{{fun {x} x}}")     =error> "arity mismatch")
(test (run "{if {< 4 5} 6 7}")  => 6)
(test (run "{if {< 5 4} 6 7}")  => 7)
(test (run "{if + 6 7}")        => 6)
(test (run "{fun {x} x}")       =error> "returned a bad value")

;; Test laziness
(test (run "{{fun {x} 1} {/ 9 0}}") => 1)
(test (run "{{fun {x} 1} {{fun {x} {x x}} {fun {x} {x x}}}}") => 1)
(test (run "{bind {{x {{fun {x} {x x}} {fun {x} {x x}}}}} 1}") => 1)

;; Test lazy constructors
(test (run "{bind {{l {list 1 {/ 9 0} 3}}}
              {+ {first l} {first {rest {rest l}}}}}")
      => 4)

;; More tests for the SLUG extensions
(test (run "'foo'") => "foo")
(test (run "{with-stx 1}") =error> "bad `with-stx' syntax")
(test (run "{with-stx {with {=}
                        {{with x = val expr}
                         {bind {{x val}} expr}}
                        {{with x _ val expr}
                         1}}
              {with x = 'blah' {first {list x x}}}}")
      => "blah")

;; IO tests
(test (run-io "{fun {} 1}") =error> "expecting an IO value")
(test (run-io "{print +}")
      =error> "cannot `print' a non-string value")
(test (run-io "{read 3}")
      =error> "expecting a receiver function")
(test (run-io "{print 'foo'}") =output> "foo")
(test (run-io "{begin2 {print 'foo'} {print 'bar'}}")
      =output> "foobar")
(test input: "blah"
      (run-io "{read {fun {x} {begin2 {print x} {print x}}}}")
      =output> "blahblah")

(test
 input: "foo"
 (run-io
  "{begin2 {print 'What is your name?'}
           {read {fun {name}
                   {begin2 {print 'Your name is '''}
                           {begin2 {print name}
                                   {print '''\n'}}}}}}")
 =output> "What is your name?"
          "Your name is 'foo'\n")

;; test two macros
(test (run "{with-stx {let {}
                           {{let {{var val} ...} body}
                            {{fun {var ...} body} val ...}}}
              {with-stx {let* {}
                              {{let* {} body} body}
                              {{let* {{id1 expr1} {id expr} ...} body}
                               {let {{id1 expr1}}
                                 {let* {{id expr} ...}
                                   body}}}}
                {let* {{x 1} {y {+ x 1}}} {+ x y}}}}")
      => 3)




;; macros for I/O
(test
 input: "Foo\nfoo@bar.com"
 (run-io
  "{with-stx {do {<-}
                 {{do {id <- {read}} next more ...}
                  {read {fun {id}
                    {begin2 next 
                            {do more ...}}}}}
                 {{do {print str} next more ...}
                  {begin2 {print str}
                           {do next more ...}}}
                 {{do expr}
                  expr}}
     {do {print 'What is your name?\n'}
         {name <- {read}}
         {print 'What is your email?\n'}
         {email <- {read}}
         {print 'Your address is '''}
         {print name}
         {print ' <'}
         {print email}
         {print '>''\n'}}}")
 =output> "What is your name?\n"
          "What is your email?\n"
          "Your address is 'Foo <foo@bar.com>'\n")

;; macros for I/O and refs (note how a `do' block is treated as just a
;; value, since it is one)
(test
 (run-io
  "{with-stx {do {<-}
                 {{do {id <- {f x ...}} next more ...}
                  {f x ... {fun {id}
                             {do next more ...}}}}
                 {{do expr next more ...}
                  {begin2 expr
                          {do next more ...}}}
                 {{do expr}
                   expr}}
     {bind {{incref   {fun {b}
                        {do {curval <- {unref b}}
                            {set-ref! b {+ 1 curval}}}}}
            {printref {fun {b sfx}
                        {do {v <- {unref b}}
                            {print {number->string v}}
                            {print sfx}}}}
            {thrice   {fun {code} {do code code code}}}}
       {do {i <- {newref 0}}
           {print 'i holds: '}
           {thrice {do {incref i} {printref i ', '}}}
           {incref i} {printref i '.\n'}}}}")
 =output> "i holds: 1, 2, 3, 4.")

(test
 (run-io
  "{bind {{incref {fun {b}
                   {unref b
                     {fun {curval}
                       {set-ref! b {+ 1 curval}}}}}}}
    {newref 0
      {fun {i}
        {begin2
          {incref i}
          {begin2
            {print 'i now holds: '}
            {unref i
              {fun {v}
                {begin2 {print {number->string v}}
                        {print '\n'}}}}}}}}}")
 =output> "i now holds: 1\n")

(test (run-io "{unref + +}") =error> "expected a Ref")
(test (run-io "{unref {print 'anything'} -}") =error> "expected a Ref")
(test (run-io "{set-ref! + 1}") =error> "expected a Ref")
(test (run-io "{set-ref! {print anything} 1}") =error> "expected a Ref")


;;; ==================================================================