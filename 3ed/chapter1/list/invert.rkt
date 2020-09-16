; EOPL exercise 1.16
;
; (invert lst), where lst is a list of 2-lists (lists of length two), returns
; a list with each 2-list reversed.
;
; > (invert '((a 1) (a 2) (1 b) (2 b)))
; ((1 a) (2 a) (b 1) (b 2))

#lang racket

(define (invert lst)
  (cond ((null? lst) '())
        (else (cons (invert-2-list (car lst)) (invert (cdr lst))))))

(define (invert-2-list lst)
  (list (car lst) (cadr lst)))

(invert '((a 1) (a 2) (1 b) (2 b)))
