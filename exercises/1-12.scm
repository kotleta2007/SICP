#lang sicp

; Exercise 1.12

(define (pascal level i)
  (cond ((= i 1) 1)
        ((= i level) 1)
        (else (+ (pascal (dec level) (dec i))
                 (pascal (dec level) i)))))

(define (get-row row)
  (define (iter i)
    (if (> i row)
        '()
        (cons (pascal row i) (iter (inc i)))))
  (iter 1))

(get-row 5)
