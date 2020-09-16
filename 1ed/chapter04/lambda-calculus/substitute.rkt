#lang racket
(require "../../define-record.rkt")
(require  "../../chapter03/lambda-calculus/ast.rkt")
(require  "../../chapter03/lambda-calculus/free-bound-p.rkt")
(provide (all-defined-out))

;; Exercise 4-2.3

;; e[m/x]
(define (substitute e m x)
  (variant-case e
    (lit (datum) e)
    (varref (var) (if (eqv? var x) m e))
    (lambda (formal body)
      (if (eqv? formal x)
          e
          (cond ((not (free? x body)) e)
                ((not (free? formal m)) (make-lambda formal (substitute body m x)))
                (else (let ((new-formal (gensym 'g)))
                        (make-lambda new-formal (substitute (substitute body (make-varref new-formal) formal) m x)))))) ;; 这里为什么要parse?
      )
    (app (rator rand)
         (make-app (substitute rator m x) (substitute rand m x)))))

(module+ main
  (require  "../../chapter03/lambda-calculus/parse-with-detect-errors.rkt")
  
  (unparse (substitute (parse '(a b)) (parse 'c) 'b)) ; (a c)
  (unparse (substitute (parse '(lambda (a) (a b))) (parse 'a) 'b)) ; (lambda (g01234) (g01234 a))

  )

