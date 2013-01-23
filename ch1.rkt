#lang planet neil/sicp

; Exercise 1.1
10                                ; ==> 10
(+ 5 3 4)                         ; ==> 12
(- 9 1)                           ; ==>  8
(/ 6 2)                           ; ==>  3
(+ (* 2 4) (- 4 6))               ; ==>  6
(define a 3)                      
(define b (+ a 1))
(+ a b (* a b))                   ; ==> 19
(= a b)                           ; ==> #f
(if (and (> b a) (< b (* a b)))   
    b                             ; ==>  4
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))         ; ==> 16
      (else 25))
(+ 2 (if (> b a) b a))            ; ==>  6
(* (cond ((> a b) a)
         ((< a b) b)              ; ==> 16
         (else -1))
   (+ a 1))

; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))) ; ==> -37 / 150

; Exercise 1.3
(define (sum-of-squares x y z)
  (cond ((and (> x z)
              (> y z)) (+ (* x x) (* y y)))
        ((and (> x y)
              (> z y)) (+ (* x x) (* z z)))
        (else (+ (* y y) (* z z)))))

; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; Combination - delimited list of expressions denote a procedure.
; (operator <operands ...>)
; Compound expression - combination of primitive procedure and numbers.
; 

; Exercise 1.5 - see notebook
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
;(test 0 (p))
; An applicative-order interpreter will loop forever since (p) is recursive.
; A normal-order interpreter will return 0 since (p) is never evaluated.


(define (square x)
  (* x x))
(define (sqrt-iter guess x)
  (display guess) (newline)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

; Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x) x)))
; Since new-if is a standard function definition all of its operands are
; evaluated with applicative-order. The else-clause within sqrt-iter is a
; recursive call that will cause the interpreter to loop forever. The special
; function (if) evaluates the predicate before evaluating either the consequent
; or alternative. (if) would not cause a loop.

; Exercise 1.7
; Small numbers: The effectiveness of (sqrt) goes down the closer the radicand
; is to the tolerance.
; Large numbers: (sqrt 1e49) loops forever. (improve) returns the same guess.
; (improve 3.162277660168379e+024 1e49) => 3.162277660168379e+024
; This is the correct answer but the floating point representation of this
; number is not within a precision of 0.001.
