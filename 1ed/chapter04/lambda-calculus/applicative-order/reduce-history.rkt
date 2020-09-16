#lang racket
(require  "../../../chapter03/lambda-calculus/ast.rkt")
(require  "../../../chapter03/lambda-calculus/parse-with-detect-errors.rkt")
(require "./reduce-once-appl.rkt")
(provide (all-defined-out))

;; Exercise 4-3-1

(define (reduce-history/parsed-exp exp n)
  (if (eqv? n 0)
      '()
      (reduce-once-appl exp
                        (lambda (e)
                          (cons e (reduce-history/parsed-exp e (- n 1))))
                        (lambda ()
                          '()))))

(define (reduce-history exp n)
  (map unparse (reduce-history/parsed-exp (parse exp) n)))

(module+ main
  (reduce-history '((lambda (x) (x ((lambda (x) y) z))) w) 5)
  (reduce-history '((lambda (x) (x x)) (lambda (x) (x x))) 3)
  
  ;; '((w ((lambda (x) y) z))
  ;;   (w y))
  
  ;; '(((lambda (x) (x x)) (lambda (x) (x x)))
  ;;   ((lambda (x) (x x)) (lambda (x) (x x)))
  ;;   ((lambda (x) (x x)) (lambda (x) (x x))))


  ;; p.113 infinite loop
  (reduce-history '((lambda (y) 3)
                    ((lambda (x) (x x)) (lambda (x) (x x)))) 10)

  ;; https://cs.stackexchange.com/questions/129976/is-the-term-lambda-x-xy-y-a-normal-form-in-call-by-value-reduction-strate
  (reduce-history '((lambda (x) (x x)) (y y)) 3) ; '()

  
  )
