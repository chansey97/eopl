#lang racket
(require "../../define-record.rkt")
(require "./ast.rkt")

(define (union a b)
  (cond ((null? a) b)
        ((member (car a) b)
         (union (cdr a) b))
        (else (cons (car a)
                (union (cdr a) b)))))

;; p.86
(define free-vars
  (lambda (exp)
    (variant-case exp
      (lit (datum)
           '())
      (varref (var)
              (list var))
      (lambda (formal body)
        (remove formal (free-vars body)))
      (app (rator rand)
           (union (free-vars rator) (free-vars rand))))))

(module+ main
  (require "./parse-with-detect-errors.rkt")

  (free-vars (parse '(lambda (y) (c (lambda (z) (+ x))))))
  )

