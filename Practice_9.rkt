#lang pl 09

#|
If the last thing a routine does before it returns is call another routine, 
rather than doing a jump-and-add-stack-frame immediately followed by a 
pop-stack-frame-and-return-to-caller, it should be safe to simply jump to 
the start of the second routine, letting it re-use the first routine's 
stack frame.  
|#

;; we represent labels (goto targets) as thunks, and registers (or
;; memory locations in general) as integer boxes.
(define-type Label    = (-> Integer))
(define-type Register = (Boxof Integer))

;; "X = Y"
;; assigns the contents of register Y to register X
(: mov : Register Register -> Void)
(define (mov X Y) (set-box! X (unbox Y)))

;; "X = N"
;; assigns the constant N (an "immediate" value)  to register X
(: movi : Register Integer -> Void)
(define (movi X N) (set-box! X N))

;; "X += Y"
;; increments register X by register Y
(: add : Register Register -> Void)
(define (add X Y) (set-box! X (+ (unbox X) (unbox Y))))

;; "X += N"
;; increments register X by a constant N
(: addi : Register Integer -> Void)
(define (addi X N) (set-box! X (+ (unbox X) N)))

;; "X -= Y"
;; deccrements register X by register Y
(: sub : Register Register -> Void)
(define (sub X Y) (set-box! X (- (unbox X) (unbox Y))))

;; "X -= N"
;; decrements register X by a constant N
(: subi : Register Integer -> Void)
(define (subi X N) (set-box! X (- (unbox X) N)))

;; "X &= Y"
;; sets X to the bitwise "and" of X and Y
;;(: and : Register Register -> Void)
;;(define (and X Y) (set-box! X (bitwise-and (unbox X) (unbox Y))))

;; "X &= N"
;; sets X to the bitwise "and" of X and a constant N
(: andi : Register Integer -> Void)
(define (andi X N) (set-box! X (bitwise-and (unbox X) N)))

;; "X >>= N"
;; shifts register X right by N bits
(: shri : Register Integer -> Void)
(define (shri X N) (set-box! X (arithmetic-shift (unbox X) (- N))))

;; "goto L"
;; (goto L) jumps to the label -- labels are represented as nullary
;; functions (also called "thunks")
(: goto : Label -> Integer)
(define (goto L) (L))

;; "ret X"
;; return the contents of register X
(: ret : Register -> Integer)
(define (ret X) (unbox X))

;; "ret N"
;; return the constant N
(: reti : Integer -> Integer)
(define (reti N) N)

;; "if X=0 goto L1 else goto L2"
;; if register X is zero, jump to L1, else jump to L2
(: if0 : Register Label Label -> Integer)
(define (if0 a l1 l2) (if (zero? (unbox a)) (goto l1) (goto l2)))

;; "if X>0 goto L1 else goto L2"
;; if register X is positive, jump to L1, else jump to L2
(: ifp : Register Label Label -> Integer)
(define (ifp a l1 l2) (if (positive? (unbox a)) (goto l1) (goto l2)))

(: fib : Integer -> Integer)
;; compute the nth fibonacci number using the assembly language
(define (fib n)
  (: A : Register) (define A (box 0))
  (: B : Register) (define B (box 1))
  (: C : Register) (define C (box 0))
  (: N : Register) (define N (box n))
  ;;
  (: main : Label)
  (: step : Label)
  (: done : Label)
  ;;
  (define (main) (if0  N done step))
  (define (step) (mov  C A)
                 (add  C B)
                 (mov  A B)
                 (mov  B C)
                 (subi N 1)
                 (goto main))
  (define (done) (ret  A))
  ;;
  (main))
;; test
(test (map fib '(0 1 2 3 4 5 6 7 8 9 10))
      => '(0 1 1 2 3 5 8 13 21 34 55))

(: more-ones? : Integer Integer -> Integer)
;; returns 1 if `a' has more 1s in its binary representation than `b'
(define (more-ones? a b)
  (: A : Register) (define A (box a))
  (: B : Register) (define B (box b))
  (: C : Register) (define C (box 0))
  (: D : Register) (define D (box 0))
  
  (: main   : Label)
  (: inc    : Label)
  (: step   : Label)
  (: step2  : Label)
  (: check  : Label)
  (: swap   : Label)
  (: done   : Label)
  (: ret1   : Label)
  (: ret0   : Label)
  
  (define (main)   (mov B A)
                   (shri A 1)
                   (andi B 1)
                   (ifp B inc step))
  (define (inc)    (addi C 1)
                   (goto main))
  (define (step)   (if0 A step2 main))
  (define (step2)  (ifp C check done))
  (define (check)  (ifp D done swap))
  (define (swap)   (movi A b)
                   (mov D C)
                   (movi C 0)
                   (main))
  (define (done)   (sub D C)
                   (ifp D ret1 ret0))
  (define (ret1)   (reti 1))
  (define (ret0)   (reti 0))
  
  (main))
;; tests
(test (more-ones? 0 0) => 0)
(test (more-ones? 1 0) => 1)
(test (more-ones? 1 2) => 0)
(test (more-ones? 2 0) => 1)
(test (more-ones? 0 1) => 0)
(test (more-ones? 0 2) => 0)
(test (more-ones? 2 1) => 0)
(test (more-ones? 2 2) => 0)
(test (more-ones? 3 1) => 1)


(define minutes-spent 120)