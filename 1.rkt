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