; EOPL exercise 1.21
;
; (product sos1 sos2), where sos1 and sos2 are each a list of symbols without
; repetitions, returns a list of 2-lists that represents a Cartesian product
; of sos1 and sos2. The 2-lists can appear in any order.
;
; > (product '(a b c) '(x y))
; ((a x) (a y) (b x) (b y) (c x) (c y))

#lang racket

;; (define (product sos1 sos2)
;;   (for*/list ([i sos1]
;;               [j sos2]
;;               )
;;     (list i j)))

;; (define (flat-map proc lst)
;;   (if(null? lst)
;;      '()
;;      (append (proc (car lst))
;;              (flat-map proc (cdr lst)))))

;; (define (product sos1 sos2)
;;   (flat-map (lambda (s1)
;;               (map (lambda (s2)
;;                      (list s1 s2)) sos2)) sos1))

(define (product sos1 sos2)
  (if (null? sos1)
      '()
      (append (list-of-list-2 (car sos1) sos2)
              (product (cdr sos1) sos2))))

(define (list-of-list-2 s sos)
  (map (lambda (s2) (list s s2)) sos))

(product '(a b c) '(x y))
