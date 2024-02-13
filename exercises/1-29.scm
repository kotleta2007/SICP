#lang sicp

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (square x) (* x x))
(define (cube x) (* x x x))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (let ((y0 (f a))
          (y1 (f (+ a h)))
          (yn (f (+ a (* n h)))))
      (* (/ h 3)
         (+ y0
            (* 4 y1)
            (* 2
               (sum
                (lambda (k)
                  (+ (f (+ a (* k h)))
                     (* 2 (f (+ a (* (+ k 1) h))))))
                2
                (lambda (n) (+ n 2))
                (- n 1)))
            yn)))))

(abs (- (simpson exp 0 1 100) (- (exp 1) 1)))
(simpson cube 0 1 100)
(simpson cube 0 1 1000)
