;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;;Question 2
;;Purpose: Consumes 3 integers and determines whether 
;;they are within an interval of 2
;;Contract: integer integet integer -> boolean
(define (near? a b c)
  (<= (- (max a b c) (min a b c)) 2))

;;Tests for near?
(near? 1 -1 1)
(not (near? 2 -2 2))
(near? 1 2 3)
(not(near? 1 5 9))
(near? 1 2 3)
(not (near? 1 8 9))

;;Question 3
;;Purpose: consumes a list of symbols and 
;;returns the number of occurrences of the symbol x in the list
;;(list symbol) -> integer
(define (count-xs l)
  (cond [(empty? l) 0]
        [(equal? (first l) 'x) (+ (count-xs (rest l)) 1)]
        [else (count-xs (rest l))]))

;;tests for count-xs
(equal? (count-xs empty) 0) 
(equal? (count-xs (list 'x)) 1)
(equal? (count-xs (list 'a 'x)) 1)
(equal? (count-xs (list 'x 'x)) 2)
(equal? (count-xs (list 'a 'a)) 0)
(equal? (count-xs (list 'a 'x 'a 'x)) 2)

;;Questions 4
;;Purpose: determines whether a list of numbers is sorted in ascending order.
;;Contract: (list number) -> boolean
(define (ascending? l)
  (cond [(empty? l) true]
        [(empty? (rest l)) true]
        [(<= (first l) (second l)) (ascending? (rest l))]
        [else false]))

;;tests for ascending?
(ascending? '())
(ascending? '(0 0))
(ascending? '(1))
(ascending? '(1 2 3 4 5))
(not(ascending? '(2 0 3 4 5)))
(not(ascending? '(9 5 10 4 5)))
(not(ascending? '(9 52 10 41 -8)))


;;Question 5
;;Purpose: consumes two arguments that are lists of equal length,return a list 
;;that contains two-element lists from 
;;the first and the second lists respectively
;;Contract: (list ?) (list ?) -> (list (list ? ?))
(define (zip2 l1 l2)
  (cond [(empty? l1) empty]
        [else (cons
               (cons(first l1)(cons (first l2) empty))
               (zip2 (rest l1)(rest l2)))]))

;;test for zip2
(equal? (list (list 1 'a) (list 2 'b) (list 3 'c))
        (zip2 (list 1 2 3) (list 'a 'b 'c)))
(equal? empty (zip2 empty empty))
(equal? (list (list 1 2)) (zip2 (list 1) (list 2)))


;;Question 6
;;Purpose: define my-picture — bind it to the number next to your picture 
(define my-picture 10)

;;Purpose: define my-other-pictures as a list of numbers for 
;;all of the other picture numbers that you appear on. 
(define my-other-pictures null)


;;Question 7
;;Purpose: Define minutes-spent as the number of minutes 
;;you spent on your homework.
(define minutes-spent 120)

;;test for question 6, 7 
(equal? my-picture 10)
(not(equal? my-picture 11))

(equal? my-other-pictures null)
(not(equal? my-other-pictures '(100)))

(equal? minutes-spent 120)
(not(equal? minutes-spent 100))