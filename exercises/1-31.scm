#lang sicp

;; a.

; product (recursive)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (identity x) x)
(define (factorial n) (product identity 1 inc n))

(factorial 5)

; wallis product
(define (wallis n)
  (define (even? n)
    (cond ((= n 0) #t)
          ((= n 1) #f)
          (else (even? (- n 2)))))
  (define (next-even k)
    (if
     (even? k)
     k
     (inc k)))
  (define (prev-odd k)
    (if
     (not (even? k))
     k
     (dec k)))
  
  (define (top n)
    (product
     (lambda (k) (next-even (inc k)))
     1
     inc
     n))

  (define (bot n)
    (product
     (lambda (k) (prev-odd (+ k 2)))
     1
     inc
     n))

  (/ (top n)
     (bot n)))

; wallis tests
(define pi-over-4 (atan 1))

(define (wallis-test n)
  (abs (-
        (wallis n)
        pi-over-4)))

(wallis-test 1000)
(wallis-test 2001)

(define (wallis-pi n)
  (* 4.0 (wallis n)))

(wallis-pi 10000)

;; b.

; product (iterative)
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (fact-iter n) (product-iter identity 1 inc n))
(fact-iter 5)
  