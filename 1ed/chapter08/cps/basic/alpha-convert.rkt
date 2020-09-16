#lang racket
(require "../../../define-record.rkt")
(require "./ast.rkt")
(require "./next-symbol.rkt")
(provide (all-defined-out))

;;; Figure 8.4.5 : page 270. 271

(define alpha-convert
  (lambda (exp bvs)
    (let ((table
            (let ((pairs (map (lambda (bv)
                                (cons bv (next-symbol-right bv)))
                              bvs)))
              (lambda (sym)
                (let ((found-pair (assq sym pairs)))
                  (if (pair? found-pair)
                      (cdr found-pair)
                      sym))))))
      (variant-case exp
        (let (decls body)
          (make-let
            (map (lambda (decl)
                   (make-decl
                     (table (decl->var decl))
                     (decl->exp decl)))
                 decls)
            (beta body table)))
        (letrec (decls body)
          (make-letrec
            (map
              (lambda (decl)
                (make-decl
                  (table (decl->var decl))
                  (beta (decl->exp decl) table)))
              decls)
            (beta body table)))
        (else exp)))))

(define beta
  (lambda (exp table)
    (variant-case exp
      (lit (datum) exp)
      (varref (var) (make-varref (table var)))
      (if (test-exp then-exp else-exp)
        (make-if
          (beta test-exp table)
          (beta then-exp table)
          (beta else-exp table)))
      (proc (formals body)
        (make-proc formals
          (beta body (lambda (var)
                       (if (memq var formals) var (table var))))))
      (let (decls body)
        (make-let
          (map (lambda (decl)
                 (make-decl (decl->var decl)
                   (beta (decl->exp decl) table)))
               decls)
          (beta body (let ((vars (map decl->var decls)))
                       (lambda (var)
                         (if (memq var vars) var (table var)))))))
      (letrec (decls body)
        (let ((new-table
                (let ((vars (map decl->var decls)))
                  (lambda (var)
                    (if (memq var vars) var (table var))))))
          (make-letrec
            (map (lambda (decl)
                   (make-decl (decl->var decl)
                     (beta (decl->exp decl) new-table)))
                 decls)
            (beta body new-table))))
      (prim-app (primitive rands)
        (make-prim-app primitive
          (map (lambda (rand) (beta rand table)) rands)))
      (app (rator rands)
        (make-app (beta rator table)
          (map (lambda (rand) (beta rand table)) rands))))))
