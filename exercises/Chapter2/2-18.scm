#lang sicp

(define (reverse items)
  (define (rev-iter list1 list2)
    (if (null? list1)
        list2
        (rev-iter (cdr list1) (cons (car list1) list2))))
  (rev-iter items '()))

(reverse (list 1 4 9 16 25))
