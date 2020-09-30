#lang eopl

;; output language from the cps converter

(provide (all-defined-out))

(define-datatype cps-out-program cps-out-program?
  (cps-a-program
   (exp1 tfexp?)))

(define-datatype simple-expression simple-expression?
  (cps-const-exp
   (num number?))
  (cps-var-exp
   (var symbol?))
  (cps-diff-exp
   (simple1 simple-expression?)
   (simple2 simple-expression?))
  (cps-zero?-exp
   (simple1 simple-expression?))
  (cps-sum-exp
   (exps (list-of simple-expression?)))
  (cps-proc-exp
   (vars (list-of symbol?))
   (body tfexp?)))

(define-datatype tfexp tfexp?
  (simple-exp->exp
   (simple-exp1 simple-expression?))
  (cps-let-exp
   (var symbol?)
   (simple1 simple-expression?)
   (body tfexp?))
  (cps-letrec-exp
   (p-names (list-of symbol?))
   (b-varss (list-of (list-of symbol?)))
   (p-bodies (list-of tfexp?))
   (body tfexp?))
  (cps-if-exp
   (simple1 simple-expression?)
   (body1 tfexp?)
   (body2 tfexp?))
  (cps-call-exp
   (rator simple-expression?)
   (rands (list-of simple-expression?))))

;;;;;;;;;;;;;;;; a primitive pretty-printer ;;;;;;;;;;;;;;;;

;; exercise: Write a pretty-printer for programs in CPS-OUT.

(define cps-program->string
  (lambda (pgm)
    (write (unparse pgm))))

(define (unparse pgm)
  (cases cps-out-program pgm
         (cps-a-program (exp1) (unparse-tfexp exp1))))


(define (unparse-tfexp exp)
  (cases tfexp exp
         (simple-exp->exp (simple-exp1)
                          (unparse-simple simple-exp1))
         
         (cps-let-exp (var simple1 body)
                      `(let ((,var ,(unparse-simple simple1)))
                         ,(unparse-tfexp body)))
         
         (cps-letrec-exp (p-names b-varss p-bodies body)
                         `(letrec ,(map (lambda (p-name b-vars b-body)
                                          `(,p-name (λ ,b-vars ,(unparse-tfexp b-body))))
                                        p-names b-varss p-bodies)
                            ,(unparse-tfexp body)))
         
         (cps-if-exp (simple1 body1 body2)
                     `(if ,(unparse-simple simple1)
                          ,(unparse-tfexp body1)
                          ,(unparse-tfexp body2)))
         
         (cps-call-exp (rator rands)
                       (cons (unparse-simple rator) (map unparse-simple rands)))
         ))

(define (unparse-simple exp)
  (cases simple-expression exp
         (cps-const-exp (num) num)
         
         (cps-var-exp (var) var)
         
         (cps-diff-exp (simple1 simple2)
                       `(- ,(unparse-simple simple1) ,(unparse-simple simple2)))
         
         (cps-zero?-exp (simple1)
                        `(zero? ,(unparse-simple simple1)))
         
         (cps-sum-exp (exps)
                      (cons '- (map unparse-simple exps)))
         
         (cps-proc-exp (vars body)
                       `(λ ,vars ,(unparse-tfexp body)))
         ))
