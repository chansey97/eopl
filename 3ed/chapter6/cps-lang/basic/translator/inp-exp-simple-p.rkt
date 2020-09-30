#lang eopl
(require "../cps-in-lang.rkt")
(provide (all-defined-out))

;; inp-exp-simple? : InpExp -> Bool
;; returns #t or #f, depending on whether exp would be a 
;; simple-exp if reparsed using the CPS-OUT language.
(define inp-exp-simple?
  (lambda (exp)
    (cases expression exp
      (const-exp (num) #t)
      (var-exp (var) #t)
      (diff-exp (exp1 exp2)
                (and
                 (inp-exp-simple? exp1)
                 (inp-exp-simple? exp2)))
      (zero?-exp (exp1)
                 (inp-exp-simple? exp1))
      (proc-exp (ids exp) #t)
      (sum-exp (exps)
               (all-simple? exps))
      (else #f))))

(define all-simple?
  (lambda (exps)
    (if (null? exps)
        #t
        (and (inp-exp-simple? (car exps))
             (all-simple? (cdr exps))))))

