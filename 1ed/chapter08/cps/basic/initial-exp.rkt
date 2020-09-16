#lang racket
(require "../../../define-record.rkt")
(require "./simple-p.rkt")
(provide (all-defined-out))

;; Figure 8.4.1 : page 261 finding the leftmost initial expression.
(define initial-exp
  (lambda (exp)
    (letrec
      ((loop (lambda (ls)
               (cond
                 ((null? ls) exp)
                 ((simple? (car ls)) (loop (cdr ls)))
                 (else (initial-exp (car ls)))))))
      (loop (head-exps exp)))))


(module+ main
  (require "./parse.rkt")
  
  (initial-exp (parse '(if (null? x) (f x y) z)))
  (initial-exp (parse ' (g (f a b)
                           (h (p x y))
                           (if (null? x)
                               (f x y)
                               z))))
  )
