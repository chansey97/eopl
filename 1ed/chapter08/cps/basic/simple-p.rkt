#lang racket
(require "../../../define-record.rkt")
(require "./ast.rkt")
(provide (all-defined-out))

;;; Figure 8.3.2 : page 256

(define simple?
  (lambda (exp)
    (and (not (app? exp))
         (andmap simple? (head-exps exp))
         (andmap simple? (tail-exps exp)))))

(define head-exps
  (lambda (exp)
    (variant-case exp
      (lit (datum) '())
      (varref (var) '())
      (if (test-exp then-exp else-exp) (list test-exp))
      (proc (formals body) '())
      (let (decls body) (map decl->exp decls))
      (letrec (decls body) (map decl->exp decls))
      (prim-app (primitive rands) rands)
      (app (rator rands) (cons rator rands)))))

(define tail-exps
  (lambda (exp)
    (variant-case exp
      (lit (datum) '())
      (varref (var) '())
      (if (test-exp then-exp else-exp) (list then-exp else-exp))
      (proc (formals body) '())
      (let (decls body) (list body))
      (letrec (decls body) (list body))
      (prim-app (primitive rands) '())
      (app (rator rands) '()))))

(define binding-vars
  (lambda (exp)
    (variant-case exp
      (let (decls body) (map decl->var decls))
      (letrec (decls body) (map decl->var decls))
      (else '()))))

(module+ main
  (require "./parse.rkt")

  ;; p.255
  (simple? (parse '(car x)))
  (simple? (parse '(if p x (car (cdr x)))))
  (simple? (parse '(f (car x))))
  (simple? (parse '((car (f x)))))
  (simple? (parse '(if p x (f (cdr x)))))
  (simple? (parse '(if (f x) x (f (cdr x)))))
  (simple? (parse '(lambda (x) (f x))))
  (simple? (parse '(lambda (x) (car (f x)))))
  
  ;; ;; Exercise 8.3.3
  (simple? (parse '(car (f (cdr x)))))
  (simple? (parse '(f (car (cdr x)))))
  (simple? (parse '(if (zero? x) (car y) (car (cdr y)))))
  (simple? (parse (let ((x (lambda (x) x))) (cons x '()))))
  (simple? (parse '((let ((f (lambda (x) x))) (f 3)))))

  ;; TODO:
  ;; I haven't implemented case else syntax
  ;; (simple? (parse '(case (f x) ((a) (g x)) (else y))))
  
  
  )
