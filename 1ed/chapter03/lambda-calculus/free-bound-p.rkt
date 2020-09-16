#lang racket
(require "../../define-record.rkt")
(require "./ast.rkt")
(provide (all-defined-out))

;; Exercise 3.4.5

(define free?
  (lambda (var_ exp)
    (variant-case exp
                  (lit (datum) #f)
                  (varref (var)
                          (eqv? var var_))
                  (lambda (formal body)
                    (and (not (eqv? var_ formal))
                         (free? var_ body)))
                  (app (rator rand) 
                       (or (free? var_ rator)
                           (free? var_ rand))))
    ))

;; (define bound?
;;   (lambda (var_ exp)
;;     ))

(module+ main
  (require "./parse-with-detect-errors.rkt")
  
  (free? 'x (parse '(lambda (y) (x (x y)))))
  
  (free? 'v (parse '(lambda (y) (x (x y)))))

  (free? 'x (parse 'x))
  
  (free? 'x (parse '(lambda (y) (c (lambda (z) (+ b))))))
  
  (free? 'x (parse '(lambda (y) (c (lambda (z) (+ x))))))
  
  (free? 'x (parse '(lambda (y) (c (lambda (x) (+ x))))))

  )

