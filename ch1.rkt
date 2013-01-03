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
; Combination - delimited list of expressions denote a procedure. (operator <operands ...>)
; Compound expression - combination of primitive procedure and numbers.
; 