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
;
; Large numbers: (sqrt 1e49) loops forever. (improve) returns the same guess.
; (improve 3.162277660168379e+024 1e49) => 3.162277660168379e+024
; This is the correct answer but the floating point representation of this
; number is not within a precision of 0.001.
; Large numbers 2: The reason (improve) is continuously called is (good-enough?)
; returns #f for a correct guess of 3.162277660168379e+024.
;
; Improved version of good-enough? compares the previous and current guess. This
; version works for very small and very large numbers (e.g.: .0000002 and 1e49).
(define (sqrt-iter-2 prev-guess guess x)
  (display guess) (newline)
  (if (good-enough?-2 prev-guess guess)
      guess
      (sqrt-iter-2 guess (improve guess x) x)))
(define (good-enough?-2 prev-guess guess)
  (< (/ (abs (- guess prev-guess)) guess) 0.001))
(define (sqrt-2 x)
  (sqrt-iter-2 0.0 1.0 x))

; Exercise 1.8: Newton's method for cube roots
(define (cubert-iter prev-guess guess x)
  (display guess) (newline)
  (if (good-enough-cubert? prev-guess guess)
      guess
      (cubert-iter guess (improve-cubert guess x) x)))
(define (improve-cubert guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
(define (good-enough-cubert? prev-guess guess)
  (< (/ (abs (- guess prev-guess)) guess) 0.001))
(define (cubert x)
  (cubert-iter 0.0 1.0 x))

; Excercise 1.9: Determine whether process is iterative or recursive using the
; substitution model.
;
; (define (+ a b)
;   (if (= a 0) b (inc (dec a) b))))
; This a recursive process with deferred (inc) operations.
;
; (define (+ a b)
;   (if (= a 0) (+ (dec a) (inc b))))
; This is an interative process with changing state variables.

; Exercise 1.10: Ackermann's function
(define (acker x y)
  (display "(acker ") (display x) (display " ") (display y) (display ")\n")
  (cond ((= y 0) (display 0) (newline) 0)
        ((= x 0) (display (* 2 y)) (newline) (* 2 y))
        ((= y 1) (display 2) (newline) 2)
        (else (display "Recur\n") (acker (- x 1) (acker x (- y 1))))))

(acker 1 10) ; =>  1024
(acker 2 4)  ; => 65536
(acker 3 3)  ; => 65536

(define (f n) (acker 0 n))  ; => 2n   (multiplication)
(define (g n) (acker 1 n))  ; => 2^n  (exponentiation)
(define (h n) (acker 2 n))  ; => 2^^n (tetration)
(define (k n) (* 5 n n))    ; => 5n^2

; Example: Counting Change
; Number of ways to change an amount using 50c, 25c, 10c, 5c, and 1c.

(define (change a n)
  (cond ((= a 0) 1)
        ((< a 0) 0)
        ((= n 0) 0)
        (else (+ (change a (- n 1))
                 (cond ((= n 5) (change (- a 50) n))
                       ((= n 4) (change (- a 25) n))
                       ((= n 3) (change (- a 10) n))
                       ((= n 2) (change (- a  5) n))
                       ((= n 1) (change (- a  1) n)))))))
                   