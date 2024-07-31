#lang sicp

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; (define (make-rat n d) (cons n d))

; (define (make-rat n d)
;   (let ((g (gcd n d)))
;     (cons (/ n g) (/ d g))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (s (if (>= (* n d) 0) 1 -1))) ; sign of n*d
    (cons (* s (abs (/ n g)))
          (abs (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(display "---\n")

(define neg-one-third-1 (make-rat 1 -3))
(define neg-one-third-2 (make-rat -1 3))

(print-rat neg-one-third-1)
(print-rat neg-one-third-2)
(equal-rat? neg-one-third-1 neg-one-third-2)
(print-rat (mul-rat neg-one-third-1 neg-one-third-2))