#lang racket
(require "../../../define-record.rkt")
(require "./ast.rkt")
(require "./simple-p.rkt")
(require "./initial-exp.rkt")
(require "./positional-substitution.rkt")
(require "./free-vars.rkt")
(require "./alpha-convert.rkt")
(require "./next-symbol.rkt")
(provide (all-defined-out))

;;; Figure 8.6.1 : page 282

(define cps-simple
  (lambda (exp)
    (variant-case exp
      (proc (formals body)
            (make-proc (append formals '(k)) (cps-exp body)))
      (else (positional-substitution exp
                                     (map (lambda (head-or-tail-exp)
                                            (cons head-or-tail-exp
                                              (cps-simple head-or-tail-exp)))
                                          (append (head-exps exp) (tail-exps exp))))))))

;;; Figure 8.6.2 : page 283

(define cps-exp
  (lambda (exp)
    (if (simple? exp)
        (c-simple exp)
        (let ((init-exp (initial-exp exp)))
          (if (app? init-exp)
              (if (eq? init-exp exp)
                  (c-eta init-exp)
                  (c-app init-exp exp))
              (c-special init-exp exp))))))

;;; Figure 8.6.4 : page 284

(define c-simple
  (lambda (exp)
    (make-app (make-varref 'k) (list (cps-simple exp)))))

(define c-app
  (lambda (init-exp exp)
    (make-app (cps-simple (app->rator init-exp))
              (append (map cps-simple (app->rands init-exp))
                      (list (let ((g (next-symbol-left (cps-simple (app->rator init-exp)))))
                              (make-proc (list g)
                                         (cps-exp
                                          (positional-substitution exp
                                                                   (list (cons init-exp (make-varref g))))))))))))

(define c-eta
  (lambda (init-exp)
    (make-app (cps-simple (app->rator init-exp))
              (append 
               (map cps-simple (app->rands init-exp))
               (list (make-varref 'k))))))

(define (intersect a b)
  (cond ((or (null? a) (null? b)) '())
        ((member (car a) b)
         (cons (car a) (intersect (cdr a) b)))
        (else (intersect (cdr a) b))))

(define c-special  ; improved for second printing
  (lambda (init-exp exp)
    (let ((new-init-exp (alpha-convert init-exp
                                       (intersect
                                        (binding-vars init-exp)
                                        (free-vars exp)))))
      (positional-substitution new-init-exp
                               (append
                                (map (lambda (h)
                                       (cons h (cps-simple h)))
                                     (head-exps new-init-exp))
                                (map (lambda (t)
                                       (cons t (cps-exp
                                                (positional-substitution exp
                                                                         (list (cons init-exp t))))))
                                     (tail-exps new-init-exp)))))))


(module+ main
  (require "./parse.rkt")


  )

