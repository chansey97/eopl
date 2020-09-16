#lang racket
(require "../../define-record.rkt")
(require  "../../chapter03/lambda-calculus/ast.rkt")
(require  "../../chapter03/lambda-calculus/free-bound-p.rkt")
(provide (all-defined-out))

; Exercise 4.2.5 p.108
(define (eta-redex? exp)
  (if (lambda? exp)
      (let ((x (lambda->formal exp))
            (body (lambda->body exp)))
        (variant-case body
          (app (rator rand)
               (if (and (varref? rand)
                        (eqv? (varref->var rand) x))
                   (not (free? x rator))
                   #f))
          (else #f)))
      #f))

(module+ main
  (require  "../../chapter03/lambda-calculus/parse-with-detect-errors.rkt")
  
  (eta-redex? (parse '(lambda (x) (y x)))) ; #t
  (eta-redex? (parse '(lambda (x) ((lambda (y) y) x)))) ; #t
  (eta-redex? (parse '((lambda (x) (y x)) z))) ; #f
  (eta-redex? (parse '(lambda (x) ((lambda (y) x) x)))) ; #t
  )
