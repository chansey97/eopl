; EOPL exercise 1.17
;
; (down lst) wraps parenthesis aroudn each top-level elements of lst.
;
; > (down '(1 2 3))
; ((1) (2) (3))
; > (down '((a) (fine) (idea)))
; (((a)) ((fine)) ((idea)))
; > (down '(a (more (complicated)) object))
; ((a) ((more (complicated))) (object))

#lang racket

(define (down lst)
  (map list lst))

(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))
