#lang racket

;; number-elements : List → Listof(List(Int, SchemeVal))
(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))

;; The second argument tells us the position of the sublist in the original list.
;; This need not decrease at a recursive call indeed it grows, because we are passing over another element of the original list.
;; We sometimes call this a context argument or inherited attribute.

;; number-elements-from : Listof(SchemeVal) × Int → Listof(List(Int, SchemeVal))
;; usage: (number-elements-from ’(v0 v1 v2 ...) n) = ((n v0) (n+1 v1) (n+2 v2) ...)
(define number-elements-from
  (lambda (lst n)
    (if (null? lst) '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1))))))

(number-elements '(a b c d e))
