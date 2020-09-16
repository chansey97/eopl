#lang racket
(require "../../../define-record.rkt")
(require "./ast.rkt")
(provide (all-defined-out))

(define prim-ops
  '(+ - * / > < = eq? null? symbol? number? pair? zero? and or not cons car cdr list))

(define parse
  (lambda (datum)
    (match datum
      
      [x #:when (or (number? x) (boolean? x))
         (make-lit x)]

      [`(quote ,_)
       (make-lit datum)]
      
      [x #:when (symbol? x)
         (make-varref x)]

      [`(if ,test-exp ,then-exp, else-exp)
       (make-if (parse test-exp) (parse then-exp) (parse else-exp))]
      
      [`(,(or 'λ 'lambda) ,formals ,body)
       #:when (andmap symbol? formals)
       (make-proc formals (parse body))]
      
      [`(let ,decls ,body)
       (make-let (map parse-decl decls) (parse body))]
      
      [`(letrec ,decls ,body)
       (make-letrec (map parse-decl decls) (parse body))]

      [`(,rator . ,rands)
       #:when (memq rator prim-ops)
       (make-prim-app rator (map parse rands))]
      
      [`(,rator . ,rands)
       (make-app (parse rator) (map parse rands))]

      [_
       (error "parse: Invalid concrete syntax" datum)]
      )))

(define parse-decl
  (lambda (datum)
    (match datum
      
      [`(,var ,exp)
       #:when (symbol? var)
       (make-decl var (parse exp))
       ]
      
      [_
       (error "parse-decl: Invalid concrete syntax" datum)])
    ))

(define unparse
  (lambda (exp)
    (variant-case exp
      
      (lit (datum) datum)
      
      (varref (var) var)
      
      (if (test-exp then-exp else-exp)
          `(if ,(unparse test-exp) ,(unparse then-exp) ,(unparse else-exp)))
      
      (proc (formals body)
            `(λ ,formals ,(unparse body)))
      
      (let (decls body)
        `(let ,(map (λ (decl) (list (decl->var decl) (unparse (decl->exp decl)))) decls) ,(unparse body)))
      
      (letrec (decls body)
        `(letrec ,(map (λ (decl) (list (decl->var decl) (unparse (decl->exp decl)))) decls) ,(unparse body)))

      
      (prim-app (primitive rands)
                (cons primitive (map unparse rands)))
      
      (app (rator rands)
           (cons (unparse rator) (map unparse rands)))
      
      (else
       (error "unparse: Invalid abstract syntax" exp)))))

(module+ main

  (unparse (parse 1))
  
  (unparse (parse 'a))

  (unparse (parse ''(1 2 3)))
  
  (unparse (parse '(my-proc 1 2 '())))
  
  (unparse (parse '(my-proc 1 2 '(1 2 3))))
  
  (unparse (parse '(λ (x) (abc d))))

  (unparse (parse '((λ (x) (a b)) c)))
  
  (unparse (parse '(a b c)))
  
  (unparse (parse '(λ))) ; Note: There is no keywords here, so this is just a proc call, '#(app #(varref lambda) ())

  (unparse (parse '(+ 1 2 3)))

  (unparse (parse '(if (test-exp a b c)
                       (else-exp c d e)
                       (alternative f g))))
  
  (unparse (parse '(let ((var1 ((λ (x) (a b)) c))
                         (var2 (λ (x) (abc d)))
                         (var3 (let ((var1 exp1)
                                     (var2 exp2))
                                 body)))
                     (body u v w))))
  
  (unparse (parse '(letrec ((var1 ((λ (x) (a b)) c))
                            (var2 (λ (x) (abc d)))
                            (var3 (let ((var1 exp1)
                                        (var2 exp2))
                                    body)))
                     (body u v w))))

  (unparse (parse '(letrec ((x 3)
                            (y (lambda () (+ x 1))))
                     (y))))
  
  ;; TODO:
  ;; According to p.72, this should be illegal,
  ;; but I haven't implemented any semantic checking.
  (parse '(letrec ((x 3)
                   (y (+ x 1)))
            y))

  )

