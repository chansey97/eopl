; EOPL exercise 1.25
;
; (exists? number? '(a b c 3 e)) returns #t if any element of lst satisfies
; pred, and returns #f otherwise.
;
; > (exists? number? '(a b c 3 e))
; #t
; > (exists? number? '(a b c d e))
; #f

#lang racket

(define (exists? pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
          #t
          (exists? pred (cdr lst)))))

(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))
(exists? number? '())
