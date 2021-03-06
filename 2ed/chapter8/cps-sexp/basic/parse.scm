(define-datatype program program?
  (a-program
   (a-program34 expression?)))

(define-datatype primitive primitive?
  (plus-prim)
  (minus-prim)
  (mult-prim)
  (incr-prim)
  (decr-prim)
  (zero?-prim)
  (car-prim)
  (cdr-prim)
  (cons-prim)
  (empty-prim)
  (null?-prim)
  (list-prim))

(define sym-prim-tab
  (list
   (list '+ plus-prim)
   (list '+ plus-prim)
   (list '- minus-prim)
   (list '* mult-prim)
   (list 'add1 incr-prim)
   (list 'sub1 decr-prim)
   (list 'zero? zero?-prim)
   (list 'car car-prim)
   (list 'cdr cdr-prim)
   (list 'cons cons-prim)
   (list 'emptylist empty-prim)
   (list 'null? null?-prim)
   (list 'list list-prim)))

(define-datatype expression expression?
  (lit-exp (lit-exp35 number?))
  (var-exp (var-exp36 symbol?))
  (primapp-exp
   (primapp-exp37 primitive?)
   (primapp-exp38 (list-of expression?)))
  (if-exp
   (if-exp39 expression?)
   (if-exp40 expression?)
   (if-exp41 expression?))
  (let-exp
   (let-exp42 (list-of symbol?))
   (let-exp43 (list-of expression?))
   (let-exp44 expression?))
  (proc-exp
   (proc-exp45 (list-of symbol?))
   (proc-exp46 expression?))
  (app-exp
   (app-exp47 expression?)
   (app-exp48 (list-of expression?)))
  (letrec-exp
   (letrec-exp49 (list-of symbol?))
   (letrec-exp50 (list-of (list-of symbol?)))
   (letrec-exp51 (list-of expression?))
   (letrec-exp52 expression?)))

(define (parse datum)
  (a-program (parse-exp datum)))

(define parse-exp
  (lambda (datum)
    (pmatch datum

      (,x
       (guard (symbol? x))
       (var-exp x))

      (,x
       (guard (number? x))
       (lit-exp x))

      ((,prim . ,exps)
       (guard (assq prim sym-prim-tab))
       (primapp-exp ((cadr (assq prim sym-prim-tab))) (map parse-exp exps)))
      
      ((lambda ,vars ,body)
       (proc-exp vars (parse-exp body)))

      ((if ,exp1 ,exp2, exp3)
       (if-exp (parse-exp exp1) (parse-exp exp2) (parse-exp exp3)))

      ((let ,decls ,body)
       (let ((decls (map parse-let-decl decls)))
         (let-exp (map car decls)
                     (map cadr decls)
                     (parse-exp body))))

      ((letrec ,decls ,body)
       (let ((decls (map parse-letrec-func-decl decls)))
         (letrec-exp (map car decls)
                     (map cadr decls)
                     (map caddr decls)
                     (parse-exp body))))

      ((,rator . ,rands)
       (app-exp (parse-exp rator) (map parse-exp rands)))

      (_
       (eopl:error "parse-exp: Invalid concrete syntax" datum))
      )))

(define parse-letrec-func-decl
  (lambda (datum)
    (pmatch datum
      ((,name (lambda ,formals ,body))
       (list name formals (parse-exp body))
       )
      (_
       (error "parse-letrec-func-decl Invalid concrete syntax" datum)))
    ))

(define parse-let-decl
  (lambda (datum)
    (pmatch datum
      ((,var ,exp)
       (list var (parse-exp exp))
       )
      (_
       (error "parse-let-decl Invalid concrete syntax" datum)))
    ))
