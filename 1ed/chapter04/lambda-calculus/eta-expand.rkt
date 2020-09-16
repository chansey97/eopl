#lang racket
(require "../../define-record.rkt")
(require  "../../chapter03/lambda-calculus/ast.rkt")
(provide (all-defined-out))

;; Exercise 4-2-7
;; Write a procedure eta-expand that takes a parsed expression exp (that may
;; be presumed to denote a function of one argument) and returns a parsed
;; expression of the form (for some x not free in exp)

;; (lambda (x) (exp x))

(define (eta-expand exp)
  (let ((x (gensym 'g)))
    (make-lambda x (make-app exp (make-varref x)))))

(module+ main
  (require  "../../chapter03/lambda-calculus/parse-with-detect-errors.rkt")
  
  (unparse (eta-expand (parse '(lambda (y) y))))
  (unparse (eta-expand (parse '((lambda (v) (lambda (y) (v y)))
                                (lambda (x) x)))))
  
;; '(lambda (g23801) ((lambda (y) y) g23801))
;; '(lambda (g23802)
;;    (((lambda (v) (lambda (y) (v y)))
;;      (lambda (x) x))
;;     g23802))
  
  )
