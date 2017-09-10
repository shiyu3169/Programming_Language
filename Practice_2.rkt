#lang pl 02
;;Question 1 a
;;Write a BNF for “LE”: a similarly simple language of “List Expressions”.

#|
<BS> ::= <num>
       | <sym>

<LB> :: = <null>
        | { cons <BS> <LB>  }
        | { cons <LB> <LB>  }
        | { list { U <LB> <BS> } ... }
        | { append <LB> ... }

<LE> :: = <LB>
        | <BS>
|#

;;Question 1 b

;; ** The AE is changed from prefix into infix by moving symbols
#| BNF for the AE language:
   <AE> ::= <num>
          | {<AE> + <AE> }
          | {<AE> - <AE> }
          | {<AE> * <AE> }
          | {<AE> / <AE> }
|#

;; AE abstract syntax trees
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE)
;; to convert s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    ;; we change the following 4 cases from prefix into infix for match
    [(list lhs '+ rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '- rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '* rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '/ rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> AE)
;; parses a string containing an AE expression to an AE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: eval : AE -> Number)
;; consumes an AE and computes the corresponding number
(define (eval expr)
  (cases expr
    [(Num n)   n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (let ([d (eval r)])  ;;added a condition that a number / 0 = 999
               (if (zero? d)        
                   999
                    (/ (eval l) (eval r))))]))

(: run : String -> Number)
;; evaluate an AE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "3") => 3)
(test (run "{3 + 4}") => 7)
(test (run "{{3 - 4} + 7}") => 6)
(test (run "{1 / 2}") => 1/2)
(test (run "{5 / 0}") => 999)
(test (run "{5 * 1}") => 5)
(test (run "{{5 * 0} + 1}") => 1)
(test (run "{syntax is wired}") =error> "bad syntax")

;;Question 1c

#|
1. 
The problem is that the get and set are on the same level of arithmetic express
, which means it will be ambiguous, because we have no control of get
 In the given example, the get may get nothing or junk,
because get is the same level as other arithmetic express. 
And it is ambiguous to match 2 sets with 1 get.
Also a function may begin with a get or {set (get)}, which doesn't make sense.

I suggested to add a non-terminals to seperate get and other arithmetic express
to avoid get junk or nothing. Then there won't be ambigous.
|#

#|
2.
<AE> ::= <num>
       | { + <AE> <AE> }
       | { - <AE> <AE> }
       | { * <AE> <AE> }
       | { / <AE> <AE> }

<GAE> :: = <AE>
         | { + { set <GAE> } get }
         | { - { set <GAE> } get }
         | { * { set <GAE> } get }
         | { / { set <GAE> } get }
         | { set <GAE> }

<MAE> :: = { seq <AE> }
         | { seq <GAE> ... get } 
|#

;;Question 2
(: square : Number -> Number)
;;Consume a number and Calculate the square of the number 
(define (square n)
  (* n n))

(test (square 1) => 1)
(test (square 2) => 4)
(test (square 0) => 0)

(: sum-of-squares : (Listof Number) -> Number)
;;Use foldl together with map to define a sum-of-squares function 
;;which takes a list of numbers as input, and produces a number 
;;which is the sum of the squares of all of the numbers in the list.
(define (sum-of-squares l)
  (foldl + 0 (map square l)))

(test (sum-of-squares (list 1 2 3)) => 14)
(test (square 0) => 0)
(test (sum-of-squares (list -1 -2 -3)) => 14)


;;Question 3

;;a
;; BINTREE abstract syntax trees
(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE])

;;b
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
;;write a (higher-order) function called tree-map that 
;;takes in a numeric function f and a binary tree, and returns a tree 
;;with the same shape but using f(n) for values in its leaves.
(define (tree-map f t)
  (: f : Number -> Number)
  (cases t 
    [(Leaf n) (Leaf (f n))]
    [(Node l r) (Node (tree-map f l) (tree-map f r))]))

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
      => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))

(test (tree-map square (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
      => (Node (Leaf 1) (Node (Leaf 4) (Leaf 9))))

(test (tree-map sub1 (Leaf 10))
      => (Leaf 9))

;;c
;;implement tree-fold, which is the equivalent of the swiss-army-knife tool 
;;that foldl is for lists. Tree-fold is for BINTREE to do something with given 
;; BINTREE and function
(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold fc fl t)
  (cases t
    [(Leaf n) (fl n)]
    [(Node l r) (fc (tree-fold fc fl l) (tree-fold fc fl r))]))


;;test
(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
  (tree-fold (inst append Number) (inst list Number) tree))

(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (list 1 2 3))
(test (tree-flatten (Leaf 10)) => (list 10))
(test (tree-flatten (Node (Node (Leaf 2) (Leaf 3)) (Leaf 1))) => (list 2 3 1))


;;d
;;consumes a tree and returns a tree that is its mirror image.
(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse b)
  (: rev-helper : BINTREE BINTREE -> BINTREE)
  (define (rev-helper l r)
    (Node r l))
  (tree-fold rev-helper Leaf b))


;;test
(test (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))) =>
        (tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))))

(test (reverse (tree-flatten (Leaf 10))) =>
        (tree-flatten (tree-reverse (Leaf 10))))


;;Question 4
(define minutes-spent 600)




