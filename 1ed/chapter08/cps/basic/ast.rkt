#lang racket
(require "../../../define-record.rkt")
(provide (all-defined-out))

(define-record lit (datum))
(define-record varref (var))
(define-record if (test-exp then-exp else-exp))
(define-record proc (formals body)) ; multiple formals
(define-record let (decls body))
(define-record letrec (decls body))
(define-record app (rator rands)) ; multiple rands
(define-record prim-app (primitive rands))
(define-record decl (var exp))
