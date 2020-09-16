#lang racket
(require "../../define-record.rkt")
(require  "../../chapter03/lambda-calculus/ast.rkt")
(require "./eta-redex-p.rkt")
(provide (all-defined-out))

;; (lambda (x) (E x)) = E
(define (eta-reduce exp)
  (if (eta-redex? exp)
      (let ((x (lambda->formal exp))
            (e (app->rator (lambda->body exp))))
        e)
      (error "eta-reduce Not a eta-redex" exp)))

(module+ main
  (require  "../../chapter03/lambda-calculus/parse-with-detect-errors.rkt")
  
  (unparse (eta-reduce (parse '(lambda (x) ((lambda (y) y) x))))) ; '(lambda (y) y)
  )
