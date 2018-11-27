; EOPL exercise 1.18
;
; (swapper s1 s2 slist) returns a list the same as slist, but with all
; occurences of s1 replaced by s2 and all occurences of s2 replaced by s1.
;
; > (swapper 'a 'd '(a b c d))
; (d b c a)
; > (swapper 'a 'd '(a d () c d))
; (d a () c a)
; > (swapper 'x 'y '((x) y (z (x))))
; ((y) x (z (y)))

#lang racket

(define (swapper s1 s2 slist)
  (cond ((null? slist) '())
        (else (cons (swapper-in-s-exp s1 s2 (car slist)) (swapper s1 s2 (cdr slist))))))

(define (swapper-in-s-exp s1 s2 sexp)
  (cond ((symbol? sexp) (check-and-swap s1 s2 sexp))
        (else (swapper s1 s2 sexp))))

(define (check-and-swap s1 s2 sexp)
  (cond ((eqv? sexp s1) s2)
        ((eqv? sexp s2) s1)
        (else sexp)))

(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))
