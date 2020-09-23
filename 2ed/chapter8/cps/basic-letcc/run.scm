;;; **************** top-level ****************

(define run
  (lambda (string)
    (unparse (cps-of-program (scan&parse string)))))
