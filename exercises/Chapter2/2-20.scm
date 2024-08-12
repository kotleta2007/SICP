#lang sicp

(define (same-parity first . rest)
  (let ((matches-parity? (lambda (n) (= (modulo first 2) (modulo n 2)))))
    (define (iter items)
      (cond ((null? items) '())
            ((matches-parity? (car items)) (cons (car items) (iter (cdr items))))
            (else (iter (cdr items)))))
    (cons first (iter rest))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
