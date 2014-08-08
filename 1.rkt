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