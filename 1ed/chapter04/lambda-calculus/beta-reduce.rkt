#lang racket
(require "../../define-record.rkt")
(require  "../../chapter03/lambda-calculus/ast.rkt")
(require  "./beta-redex-p.rkt")
(require  "./substitute.rkt")
(provide (all-defined-out))

; Exercise 4.2.4

;; ((lambda (x) E) M) = E [M/x]
(define (beta-reduce exp)
  (if (beta-redex? exp)
      (let ((x (lambda->formal (app->rator exp)))
            (e (lambda->body (app->rator exp)))
            (m (app->rand exp)))
        (substitute e m x))
      (error "beta-reduce: Not a beta-redex" exp)))


(module+ main
  (require  "../../chapter03/lambda-calculus/parse-with-detect-errors.rkt")
  
  (unparse (beta-reduce (parse '((lambda (x) (y x)) z)))) ; '(y z)
  
  (unparse (beta-reduce (parse '((lambda (x) (lambda (y) (x y))) ; '(lambda (g26081) ((y w) g26081))
                                 (y w)))))

  (unparse (beta-reduce (parse '((lambda (x) (x x)) (lambda (x) (x x)))))) ; '((lambda (x) (x x)) (lambda (x) (x x)))
  )
