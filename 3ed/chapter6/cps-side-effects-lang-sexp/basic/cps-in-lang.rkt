#lang racket
(require eopl)

;; input language for the CPS converter, based on EXPLICIT-REFS

(provide (all-defined-out))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (var-exp
   (var symbol?))
  (proc-exp
   (vars (list-of symbol?))
   (body expression?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (sum-exp
   (exps (list-of expression?)))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?))
  (letrec-exp
   (ids (list-of symbol?))
   (bidss (list-of (list-of symbol?)))
   (proc-bodies (list-of expression?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?)))
  (print-exp
   (rator expression?))
  (newref-exp
   (exp1 expression?))
  (deref-exp
   (exp1 expression?))
  (setref-exp
   (exp1 expression?)
   (exp2 expression?)))

(struct letrec-func (name formals body))

(define (parse datum)
  (a-program (parse-exp datum)))

(define parse-exp
  (lambda (datum)
    (match datum

      [x #:when (symbol? x)
         (var-exp x)]

      [x #:when (number? x)
         (const-exp x)]

      [`(,(or 'λ 'lambda) ,vars ,body)
       #:when (andmap symbol? vars)
       (proc-exp vars (parse-exp body))]

      [`(- ,exp1 ,exp2)
       (diff-exp (parse-exp exp1) (parse-exp exp2))]

      [`(+ . ,exps)
       (sum-exp (map parse-exp exps))]
      
      [`(zero? ,exp1)
       (zero?-exp (parse-exp exp1))]
      
      [`(if ,exp1 ,exp2, exp3)
       (if-exp (parse-exp exp1) (parse-exp exp2) (parse-exp exp3))]

      [`(let ((,var ,exp1)) ,body)
       (let-exp var (parse-exp exp1) (parse-exp body))]

      ;; convert 
      ;; letrec fun1 (x y z) = (+ 1 2 3 ) in body
      ;; to
      ;; (let ((fun1 (lambda (x y z) (+ 1 2 3 )))) body)

      [`(letrec ,decls ,body)
       (let ((funcs (map parse-letrec-func decls)))
         (letrec-exp (map letrec-func-name funcs)
                     (map letrec-func-formals funcs)
                     (map letrec-func-body funcs)
                     (parse-exp body)))]

      [`(print ,exp1)
       (print-exp (parse-exp exp1))]
      
      [`(newref ,exp1)
       (newref-exp (parse-exp exp1))]
      
      [`(deref ,exp1)
       (deref-exp (parse-exp exp1))]
      
      [`(setref ,exp1 ,exp2)
       (setref-exp (parse-exp exp1) (parse-exp exp2))]
      
      [`(,rator . ,rands)
       #:when (and (not (eq? rator '-))
                   (not (eq? rator '+))
                   (not (eq? rator 'zero?))
                   (not (eq? rator 'print))
                   (not (eq? rator 'newref))
                   (not (eq? rator 'deref))
                   (not (eq? rator 'setref)))
       (call-exp (parse-exp rator) (map parse-exp rands))]

      [_
       (error "parse-exp: Invalid concrete syntax" datum)]
      )))

(define parse-letrec-func
  (lambda (datum)
    (match datum
      [`(,name (,(or 'λ 'lambda) ,formals ,body))
       #:when (and (symbol? name) (andmap symbol? formals))
       (letrec-func name formals (parse-exp body))
       ]
      [_
       (error "parse-letrec-func Invalid concrete syntax" datum)])
    ))

(define scan&parse
  (lambda (string)
    (parse (read (open-input-string string)))))

(module+ main
  (require racket/pretty)

  
  (pretty-print (parse '(letrec ((fun1 (lambda (x y z) (+ x y z)))
                                 (fun2 (lambda (x y) (- x y))))
                          (zero? (- (func1 1 2 3) (func2 1 2 3))) )))
  
  ;; (pretty-print (parse '(letrec ((fun1 (x y))) ;; should error
  ;;                         1)))

  (pretty-print (parse '(let ((var11 exp12))
                          body13)))

  ;; (pretty-print (parse '(- 1 2 3))) ;; should error
  (pretty-print (parse '(+ 1 2 3)))
  ;; (pretty-print (parse '(zero? 1 2 3)))  ;; should error


  ;;;;;
  (pretty-print (parse '(print 1)))
  (pretty-print (parse '(newref 1)))
  (pretty-print (parse '(deref loc)))
  ;; (pretty-print (parse '(setref 1))) ;; should error
  (pretty-print (parse '(setref loc 22)))
  
  )
