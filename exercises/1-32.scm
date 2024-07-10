#lang sicp

;; a.

(define (accumulate combiner null-value term a next b)
  (define (iter accumulator start end)
    (if (> start end)
        accumulator
        (iter (combiner accumulator (term start))
              (next start)
              end)))
  (iter null-value a b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(product (lambda (x) x) 1 inc 10)

;; b (recursive version)

(define (accumulate-rec combiner null-value term a next b)
  (define (iter start end)
    (if (> start end)
        null-value
        (combiner
         (term start)
         (iter (next start) end))))
  (iter a b))

(define (sum-rec term a next b)
  (accumulate-rec + 0 term a next b))

(define (product-rec term a next b)
  (accumulate-rec * 1 term a next b))

(product-rec (lambda (x) x) 1 inc 10)