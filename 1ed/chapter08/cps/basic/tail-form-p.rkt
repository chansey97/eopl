#lang racket
(require "../../../define-record.rkt")
(require "./ast.rkt")
(require "./simple-p.rkt")
(provide (all-defined-out))

;;; Anonymous figure : page 257 (recently added for 3rd printing)

(define tail-form?
  (lambda (exp)
    (variant-case exp
      (proc (formals body) (tail-form? body))
      (else (and (andmap (lambda (x) 
                           (and (simple? x) (tail-form? x)))
                         (head-exps exp))
                 (andmap tail-form? (tail-exps exp)))))))

(module+ main
  (require "./parse.rkt")

  ;; p.255
  (tail-form? (parse '(car x)))
  (tail-form? (parse '(if p x (car (cdr x)))))
  (tail-form? (parse '(f (car x))))
  (tail-form? (parse '((car (f x)))))
  (tail-form? (parse '(if p x (f (cdr x)))))
  (tail-form? (parse '(if (f x) x (f (cdr x)))))
  (tail-form? (parse '(lambda (x) (f x))))
  (tail-form? (parse '(lambda (x) (car (f x)))))
  
  ;; Exercise 8.3.3
  (simple? (parse '(car (f (cdr x)))))
  (simple? (parse '(f (car (cdr x)))))
  (simple? (parse '(if (zero? x) (car y) (car (cdr y)))))
  (simple? (parse (let ((x (lambda (x) x))) (cons x '()))))
  (simple? (parse '((let ((f (lambda (x) x))) (f 3)))))
  ;; (simple? (parse '(case (f x) ((a) (g x)) (else y))))  ; I have not implemented case
  )
