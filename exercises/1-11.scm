#lang sicp

; Exercise 1.11

; f(n) = n                            if n < 3
; f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) if n ≥ 3

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (f-iter-helper k prev1 prev2 prev3)
    (cond ((= k 0) prev3)
          ((= k 1) prev2)
          ((= k 2) prev1)
          (else (f-iter-helper (dec k) (+ prev1 (* 2 prev2) (* 3 prev3)) prev1 prev2))))
  (f-iter-helper n 2 1 0))

(f-rec 0)
(f-rec 1)
(f-rec 2)
(f-rec 3)
(f-rec 4)

(f-iter 0)
(f-iter 1)
(f-iter 2)
(f-iter 3)
(f-iter 4)