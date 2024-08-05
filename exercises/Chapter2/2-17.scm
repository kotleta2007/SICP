#lang sicp

(define (last-pair items)
  (cond ((null? items) (error "empty list"))
        ((null? (cdr items)) items)
        (else (last-pair (cdr items)))))

;; alternative solution using built-in list operations
(define (last-pair-list-ops items)
  (list (list-ref items (dec (length items)))))

(last-pair (list 23 72 149 34))
(last-pair-list-ops (list 23 72 149 34))
