#lang eopl
(provide (all-defined-out))

;;;;;;;;;;;;;;;; utilities ;;;;;;;;;;;;;;;;

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))

;; list-set : SchemeList * Int * SchemeVal -> SchemeList
;; returns a list lst1 that is just like lst, except that 
;; (listref lst1 n) = val.
(define list-set
  (lambda (lst n val)
    (cond
      ((null? lst) (eopl:error 'list-set "ran off end"))
      ((zero? n) (cons val (cdr lst)))
      (else (cons (car lst) (list-set (cdr lst) (- n 1) val))))))

;; list-index : (SchemeVal -> Bool) * SchemeList -> Maybe(Int)
;; returns the smallest number n such that (pred (listref lst n))
;; is true.  If pred is false on every element of lst, then returns
;; #f. 
(define list-index
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) 0)
      ((list-index pred (cdr lst)) => (lambda (n) (+ n 1)))
      (else #f))))

