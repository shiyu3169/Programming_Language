#lang pl 10

(define (Y f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (z) ((x x) z))))))


(rewrite (define/rec (f x ...) E)
         => (define f
              (let ([g (Y (lambda (f)
                            (lambda (_) ; we ignore this argument
                              (lambda (x ...)
                                (let ([f (f #f)]) 
                                  E)))))])
                (g #f))))

;;(: ackermann : Integer Integer -> Number)
;; extremely large integer for small Non-negative integers,
;;we assume the integers are positive
(define/rec (ackermann m n)
  (cond [(zero? m) (+ n 1)]
        [(zero? n) (ackermann (- m 1) 1)]
        [else      (ackermann (- m 1) (ackermann m (- n 1)))]))

(test (ackermann 3 3) => 61)
(test (ackermann 2 3) => 9)
(test (ackermann 3 2) => 29)


(rewrite (letfuns ([(f x ...) E] ...) B)
         => (let ([g (Y (lambda (funs)
                          (lambda (name)
                            (match name
                              ['f (lambda (x ...)
                                    (let ([f (funs 'f)] ...)
                                      E))]
                              ...))))])
              (let ([f (g 'f)] ...)
                B)))

(letfuns ([(even? n) (if (= n 0) #t (odd?  (- n 1)))]
          [(odd?  n) (if (= n 0) #f (even? (- n 1)))])
         (or (even? 123)
             (even? 124)))

;; an extended example
(define scan
  (letfuns ([(start str)  (loop (explode-string str) 0)]
            [(loop l n)   (match l
                            [(list)
                             (zero? n)]
                            [(cons 'open more)
                             (loop more (add1 n))]
                            [(cons 'close more)
                             (and (> n 0) (loop more (sub1 n)))]
                            [(cons (number: m) more)
                             (nums more m n)]
                            [(cons _ more)
                             (loop more n)])]
            [(nums l m n) (match l
                            [(cons (number: m1) more)
                             (and (< m m1) (nums more m1 n))]
                            [else (loop l n)])])
    start))
(test (scan "(()123(246x12)) (blah)"))
(test (not (scan "(1232)")))
(test (not (scan "()(")))
(test (not (scan "())")))

(define minutes-spent 120)