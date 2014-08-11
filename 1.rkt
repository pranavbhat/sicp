#lang scheme
;; 1.1.4: Compund procedures
;; square: x returns x*x
(define (square x)
    (* x x))

;; sum of squares
(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; -----------------------------------------------------------------------------

;; conditionals in scheme
;; (cond (p1 e1)
;;       (p2 e2)
;;       (p3 e3))
(define (abs-1 x)
  (cond ( (> x 0) x)
        ( (= x 0) x)
        ( (< x 0) (- x))))

;; else is a special literal which can be used as the last predicate for cond
;; (cond (p1, e1)
;;       (p2, e2)
;;       (else e3))
(define (abs-2 x)
  (cond ( (< x 0) (- x))
        (else x)))

;; NOTE: cond evaluation is "undefined" if none of the predicates return true.
;; -----------------------------------------------------------------------------

;; (if <predicate> <consequence> <alternative>)
;; if predicate is true, execute consequence else the alternative
(define (abs-3 x)
  (if (< x 0) (- x) x))

;; -----------------------------------------------------------------------------
;; Logical operators
;; (and <e1> <e2> <e3> ...) -> evaluates each expression and if anyone is false then
;; returns false. No further expressions are evaluated. If all of them are true
;; then the value of the last expression is returned

;; Check if x is in the range of 5 and 10 (not inclusive)
(define (check-range-1 x)
  (and (> x 5) (< x 10)))

;; (or <e1> <e2> <e3> ...)
;; Evaluates expressions from left-to-right. If any expression returns true then
;; that value is returned as the result of the expression. No further 
;; expressions are evaluated. If all expressions evaluate to false, 
;; then value of or is false

;; Check if x is in the range of 5 and 10 (inclusive aka <= and >=)
(define (check-range-2 x)
  (and (or (> x 5) (= x 5))
       (or (< x 10) (= x 10))))

;; (not <e1>)
;;------------------------------------------------------------------------------

;; We can even create new predicates that look like special forms
;; example: create >= 5 and <= 5
(define (>= x)
  (or (> x 5) (= x 5)))

(define (<= x)
  (or (< x 10) (= x 10)))

;; refine the check-range-2 function with these new predicates
(define (check-range-3 x)
  (and (>= x) (<= x)))

;; Exercises
;; -----------------------------------------------------------------------------
;; Exercise 1.3:
;; Define a procedure that takes three numbers as arguments and returns the sum
;; of the squares of the two larger numbers.
(define (greater x y )
  (cond ( (> x y) x)
        (else y)))

;; gets the first greater number of the 3 numbers
;; first-greater (x y z) ->  x | y | z
(define (first-greater x y z)
  (greater (greater x y) (greater y z)))

;; gets the second greater number of the 3 numbers
(define (second-greater x y z)
  (cond ( (= (first-greater x y z) x) (greater y z))
        ( (= (first-greater x y z) y) (greater x z))
        (else (greater x y))))
;; gets the sum of the squares of the greater 2 numbers
(define (sum-of-squares-greater x y z)
  (sum-of-squares (first-greater x y z) (second-greater x y z)))
;; -----------------------------------------------------------------------------

;; Exercise 1.4
;; We can create operands on the fly depending on conditions. 
;; if b is negative: use - operand so that it is eventually an add operator 
;; else (i.e b is positive): use + operand so that it is regular addition
;; -----------------------------------------------------------------------------
(define (a-plus-abs-b a b)
  ( (if (> b 0) + -) a b))

;; -----------------------------------------------------------------------------

;; Exercise 1.5
;; 
;; TODO: Theoritical answer. Need to re-read.

;; -----------------------------------------------------------------------------

;; 1.1.7: Square roots by newton's method
;; -----------------------------------------------------------------------------
;; Method : Guess a square-root of x. Improve the guess. 
;; New guess = (average (x/guess) guess) The guess should be at a diff of 0.0001 

;; Calculates average. 
(define (average x y)
  (/ (+ x y) 2))

;; Calculates an improved guess
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; improve the guess
(define (improve guess x)
  (average guess (/ x guess)))

;; Recursively checks if the guess is good enough
(define (sqrt-iter guess x)
  (if (good-enough? guess x) 
      guess
      (sqrt-iter (improve guess x)
                 x)))

;; (sqrt 2) -> square root of 2
(define (sqrt x)
  (sqrt-iter 1.0 x))

;;------------------------------------------------------------------------------

;; Exercise 1.6
;; Replace if with new-if. Define new-if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Answer: Use of new-if always returns false i.e. the program goes in endless loop. 
;; Reason: The way cond works is it evaluates all predicates in sequence and as soon
;; as the first predicate who returns a value or evaluates to true, the expression for
;; it is evalulated. When we use new-if, since there is only one predicate who evaluates
;; to false, it always evaluates the expression with else and hence goes in endless loop.

;;------------------------------------------------------------------------------
;; Exercise 1.7
;; (good-enough? fails for very large and very small numbers. 
;; hence we need a new function that detects if the difference between two consecutive 
;; guess is too small to be negligible. 
(define (good-guess? old new)
  (< (abs (- old new)) 0.001))

(define (sqrt-guess guess x)
  (if (good-guess? guess 
                   (improve guess x))
      (improve guess x)
      (sqrt-guess (improve guess x) x)))

(define (sqrt-better x)
  (sqrt-guess 1.0 x))
  
;;------------------------------------------------------------------------------
;; Exercise 1.8: Use newton's method for cube roots
;; better cube root guess is (x/y^2 + 2y) / 3
(define (improve-cube-root guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
  (if (good-guess? guess
                   (improve-cube-root guess x))
      (improve-cube-root guess x)
      (cube-root-iter (improve-cube-root guess x)
                 x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))