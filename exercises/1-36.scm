#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess iter)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (begin
            (display "iteration: ")
            (display iter)
            (newline)
            (display "guess: ")
            (display next)
            (newline)
            (try next (inc iter))))))
  (try first-guess 0))

(define (fixed-point-avg f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (average x y) (/ (+ x y) 2))
  (define (try guess iter)
    (let ((next (average guess (f guess))))
      (if (close-enough? guess next)
          next
          (begin
            (display "iteration: ")
            (display iter)
            (newline)
            (display "guess: ")
            (display next)
            (newline)
            (try next (inc iter))))))
  (try first-guess 0))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point-avg (lambda (x) (/ (log 1000) (log x))) 2.0)
