; EOPL exercise 1.36
;
; Write a procedure g such that number-elements from page 23 could be defined
; as
;
; (define number-elements
;   (lambda (lst)
;     (if (null? lst) '()
;       (g (list 0 (car lst)) (number-elements (cdr lst))))))

#lang racket

(define (g head tail)
  (cons head
        (map (lambda (item) (list (+ (car item) 1) (cadr item))) tail)))

(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst)))))) ; see (number-elements (cdr lst)) is a sub-problem which has been solved which starting by 0, eg: ((0 x) (1 y) ...)
