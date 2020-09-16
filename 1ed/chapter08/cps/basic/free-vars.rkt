#lang racket
(require "../../../define-record.rkt")
(require "./ast.rkt")
(provide (all-defined-out))

(define (union a b)
  (cond ((null? a) b)
        ((member (car a) b)
         (union (cdr a) b))
        (else (cons (car a)
                (union (cdr a) b)))))

(define free-vars
  (lambda (exp)
    (variant-case exp
      (lit (datum) '())
      
      (varref (var) (list var))
      
      (if (test-exp then-exp else-exp)
          (union (free-vars test-exp) (union (free-vars then-exp) (free-vars else-exp))))

      (proc (formals body)
            (remove* formals (free-vars body)))
      
      (let (decls body)
        (let ((b-vars (map decl->var decls))
              (b-exps (map decl->exp decls)))
          (union (foldr union '() (map free-vars b-exps))
                 (remove* b-vars (free-vars body)))))

      ;; Exercise 3.1.4
      (letrec (decls body)
        (let ((b-vars (map decl->var decls))
              (b-exps (map decl->exp decls)))
          (remove* b-vars (union (foldr union '() (map free-vars b-exps))
                                 (free-vars body)))))
      
      (prim-app (primitive rands) ; primitives are not free varaiables
                (foldr union '() (map free-vars rands)))
      
      (app (rator rands)
           (union (free-vars rator)
                  (foldr union '() (map free-vars rands)))))))

(module+ main
  (require "./parse.rkt")

  (free-vars (parse '(lambda (y) (c (lambda (z) (+ x))))))
  
  (free-vars (parse '(let ((a exp)
                           (b exp))
                       (a b))))
  
  (free-vars (parse '(if (null? (+ a b))
                         (let ((a exp)
                               (b exp))
                           (a b))
                         (lambda (y) (c (lambda (z) (+ x)))))))

  (free-vars (parse '(let ((x 3)
                           (y (lambda () (+ x 1))))
                       (y))))

  (free-vars (parse '(letrec ((x 3)
                              (y (lambda () (+ x 1))))
                       (y))))

  (free-vars (parse '(letrec ((x 3)
                              (y (+ x 1)))
                       y)))
  )

