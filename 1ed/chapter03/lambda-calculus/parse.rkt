#lang racket
(require "../../define-record.rkt")
(require "./ast.rkt")

;; p. 85

(define parse
  (lambda (datum)
    (cond
      ((number? datum) (make-lit datum))
      ((symbol? datum) (make-varref datum))
      ((pair? datum)
       (if (eq? (car datum) 'lambda)
           (make-lambda (caadr datum) (parse (caddr datum)))
           (make-app (parse (car datum)) (parse (cadr datum)))))
      (else (error "parse: Invalid concrete syntax" datum)))))

(define unparse
  (lambda (exp)
    (variant-case exp
                  (lit (datum) datum)
                  (varref (var) var)
                  (lambda (formal body) 
                    (list 'lambda (list formal) (unparse body)))
                  (app (rator rand) (list (unparse rator) (unparse rand)))
                  (else (error "unparse: Invalid abstract syntax" exp)))))

(module+ main
  ;; Exercise 3.4.4

  (parse '(a b c))
  (parse '(lambda))
  )


