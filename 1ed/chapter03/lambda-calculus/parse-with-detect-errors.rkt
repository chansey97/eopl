#lang racket
(require "../../define-record.rkt")
(require "./ast.rkt")
(provide (all-defined-out))

;; Exercise 3.4.4

(define parse
  (lambda (datum)
    (match datum
      [x #:when (number? x)
         (make-lit x)]
      [x #:when (symbol? x)
         (make-varref x)]
      [`(,(or 'λ 'lambda) (,x) ,body)
       (make-lambda x (parse body))]
      [`(,rator ,rand)
       (make-app (parse rator) (parse rand))]
      [_
       (error "parse: Invalid concrete syntax" datum)]
      )))

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
  
  (unparse (parse 1))
  
  (unparse (parse 'a))

  (unparse (parse '(λ (x) (abc d))))

  (unparse (parse '((lambda (x) (a b)) c)))
  
  ;; (parse '(a b c)) ; error as expected
  
  ;; (parse '(lambda)) ; error as expected
  )



