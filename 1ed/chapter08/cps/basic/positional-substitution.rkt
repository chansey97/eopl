#lang racket
(require "../../../define-record.rkt")
(require "./ast.rkt")
(provide (all-defined-out))

;;; Figure 8.4.3 : page 266

(define positional-substitution
  (lambda (exp pairs)
    (letrec
      ((loop
         (lambda (exp)
           (let ((found-pair (assq exp pairs)))
             (if (pair? found-pair)
                 (cdr found-pair)
                 (variant-case exp
                   (lit (datum) exp)
                   (varref (var) exp)
                   (if (test-exp then-exp else-exp)
                     (make-if
                       (loop test-exp)
                       (loop then-exp)
                       (loop else-exp)))
                   (proc (formals body)
                     (make-proc formals (loop body)))
                   (let (decls body)
                     (make-let
                       (map (lambda (decl)
                              (make-decl
                                (decl->var decl)
                                (loop (decl->exp decl))))
                            decls)
                       (loop body)))
                   (letrec (decls body)
                     (make-letrec
                       (map (lambda (decl)
                              (make-decl
                                (decl->var decl)
                                (loop (decl->exp decl))))
                            decls)
                       (loop body)))
                   (prim-app (primitive rands)
                     (make-prim-app primitive (map loop rands)))            
                   (app (rator rands)
                     (make-app (loop rator) (map loop rands)))))))))
      (loop exp))))

(module+ main
  (require "./parse.rkt")

  
  )
