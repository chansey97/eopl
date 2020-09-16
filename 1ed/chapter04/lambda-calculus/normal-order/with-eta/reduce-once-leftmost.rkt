#lang racket
(require "../../../../define-record.rkt")
(require "../../../../chapter03/lambda-calculus/ast.rkt")
(require "../../beta-redex-p.rkt")
(require "../../beta-reduce.rkt")
(require "../../eta-redex-p.rkt")
(require "../../eta-reduce.rkt")
(provide (all-defined-out))

;; Exercise 4.3.6
;; Modify reduce-once-leftmost to use η-reduction as well as β-reduction

(define reduce-once-leftmost
  (lambda (exp succeed fail)
    (variant-case exp
      (lit (datum) (fail))
      (varref (var) (fail))
      (lambda (formal body)
        (if (eta-redex? exp)
            (succeed
              (eta-reduce exp))
            (reduce-once-leftmost body
              (lambda (reduced-body)
                (succeed (make-lambda formal reduced-body)))
          fail)))
      (app (rator rand)
        (if (beta-redex? exp)
            (succeed
              (beta-reduce exp))
            (reduce-once-leftmost rator
              (lambda (reduced-rator)
                (succeed
                  (make-app reduced-rator rand)))
              (lambda ()
                (reduce-once-leftmost rand
                  (lambda (reduced-rand)
                    (succeed
                      (make-app rator reduced-rand)))
                  fail))))))))

