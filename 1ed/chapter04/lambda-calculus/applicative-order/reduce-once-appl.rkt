#lang racket
(require "../../../define-record.rkt")
(require  "../../../chapter03/lambda-calculus/ast.rkt")
(require "../beta-redex-p.rkt")
(require "../beta-reduce.rkt")
(provide (all-defined-out))

;;; Figure 4.3.2 : page 112

(define answer?
  (lambda (exp)
    (not (app? exp))))

(define reduce-once-appl
  (lambda (exp succeed fail)
    (variant-case exp
      (lit (datum) (fail))
      (varref (var) (fail))
      (lambda (formal body) (fail))
      (app (rator rand)
        (if (and (beta-redex? exp) (answer? rand))
            (succeed
              (beta-reduce exp))
            (reduce-once-appl rator
              (lambda (reduced-rator)
                (succeed
                  (make-app reduced-rator rand)))
              (lambda ()
                (reduce-once-appl rand
                  (lambda (reduced-rand)
                    (succeed
                      (make-app rator reduced-rand)))
                  fail))))))))
