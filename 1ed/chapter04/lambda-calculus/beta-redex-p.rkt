#lang racket
(require "../../define-record.rkt")
(require  "../../chapter03/lambda-calculus/ast.rkt")
(provide (all-defined-out))

;; Exercise 4.2.2 p.106

(define (beta-redex? exp)
  (variant-case exp
    (app (rator rand) (lambda? rator))
    (else #f)))

(module+ main
  (require  "../../chapter03/lambda-calculus/parse-with-detect-errors.rkt")
  
  (beta-redex? (parse '(lambda (x) (y z))))     ; #f 
  (beta-redex? (parse '((lambda (x) (y z)) z))) ; #t
  (beta-redex? (parse '(lambda (x) ((lambda (x) (y x)) z)))) ; #f 
  
  (beta-redex? (parse '((λ (x) x) (y y)))) ;#t
  ;; In fact, this is not beta-v-redex
  ;; https://cs.stackexchange.com/questions/129976/is-the-term-lambda-x-xy-y-a-normal-form-in-call-by-value-reduction-strate
  )

