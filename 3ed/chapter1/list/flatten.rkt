; EOPL exercise 1.27
;
; (flatten slist) returns a list of the symbols contained in slist in the
; order in which they occur when slist is printed. Intuitively, flatten
; removes all the inner parentheses from its arguments.
;
; > (flatten '(a b c))
; (a b c)
; > (flatten '((a) () (b ()) () (c)))
; (a b c)
; > (flatten '((a b) c (((d)) e)))
; (a b c d e)
; > (flatten '(a b (() (c))))
; (a b c)

#lang racket

(define (flatten slist)
  (if (null? slist)
      '()
      (append (flatten-in-s-exp (car slist)) (flatten (cdr slist)))))

(define (flatten-in-s-exp sexp)
  (if (symbol? sexp)
      (list sexp)
      (flatten sexp)))

(flatten '(a b c))
(flatten '((a) () (b ()) () (c)))
(flatten '((a b) c (((d)) e)))
(flatten '(a b (() (c))))
