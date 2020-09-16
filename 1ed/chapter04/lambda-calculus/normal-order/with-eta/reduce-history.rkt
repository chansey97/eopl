#lang racket
(require "../../../../chapter03/lambda-calculus/ast.rkt")
(require "../../../../chapter03/lambda-calculus/parse-with-detect-errors.rkt")
(require "./reduce-once-leftmost.rkt")
(provide (all-defined-out))

(define (reduce-history/parsed-exp exp n)
  (if (eqv? n 0)
      '()
      (reduce-once-leftmost exp
                            (lambda (e)
                              (cons e (reduce-history/parsed-exp e (- n 1))))
                            (lambda ()
                              '()))))

(define (reduce-history exp n)
  (map unparse (reduce-history/parsed-exp (parse exp) n)))

(module+ main
  ;; p.113 infinite loop in applicative order
  (reduce-history '((lambda (y) 3)
                    ((lambda (x) (x x)) (lambda (x) (x x)))) 10)

  ;; p.114
  (reduce-history '((lambda (x) (x (x y)))
                    ((lambda (w) w) z)) 5)

  (reduce-history '(lambda (x) ((lambda (y) y) z) ) 5)

  ;; eta
  (reduce-history '(lambda (x) (y x)) 5) ; 'y
  
  (reduce-history '(lambda (x) ((lambda (y) y) x)) 5) ; '(lambda (x) x), it differs from basic-history,rkt

  )
