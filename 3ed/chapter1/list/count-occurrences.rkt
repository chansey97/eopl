; EOPL exercise 1.20
;
; (count-occurences s slist) returns the number of occurences of s in slist.
;
; > (count-occurences 'x '((f x) y (((x z) x))))
; 3
; > (count-occurences 'x '((f x) y (((x z) () x))))
; 3
; > (count-occurences 'w '((f x) y (((x z) x))))
; 0

#lang racket

(define (count-occurrences s slist)
  (if (null? slist)
      0
      (+ (count-occurrences-in-s-exp s (car slist))
         (count-occurrences s (cdr slist)))))

(define (count-occurrences-in-s-exp s sexp)
  (if (symbol? sexp)
      (if (eqv? sexp s) 1 0)
      (count-occurrences s sexp)))

(count-occurrences 'x '((f x) y (((x z) x))))
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences 'w '((f x) y (((x z) x))))
