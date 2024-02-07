#lang sicp

; Exercise 1.3

; the sum of the squares of the two larger numbers
(define (sum-sq-larger x y z)
  (define (square x) (* x x))
  (define maxs
    (cond ((and (<= x y) (<= x z)) (cons y z))   ; x is smallest
          ((and (<= y x) (<= y z)) (cons x z))   ; y is smallest
          ((and (<= z x) (<= z y)) (cons x y)))) ; z is smallest
  
  (+ (square (car maxs)) (square (cdr maxs))))

(sum-sq-larger 2 3 4)
(sum-sq-larger 3 4 2)
(sum-sq-larger 3 2 4)