#lang racket

;; Exercise 2.3.2

(define free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
       (and
        (not (eqv? var (car (cadr exp))))
        (free? var (caddr exp))))
      (else
       (or
        (free? var (car exp))
        (free? var (cadr exp)))))))

(module+ main
  (free? 'x 'x)
  (free? 'x 'y)
  (free? 'x '(lambda (x) (x y)))
  (free? 'x '(lambda (y) (x y)))
  (free? 'x '((lambda (x) x) (x y)))
  (free? 'x '(lambda (y) (lambda (z) (x (y z)))))

  )
