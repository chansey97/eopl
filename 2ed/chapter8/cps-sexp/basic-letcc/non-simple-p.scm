;;; **************** syntactic stuff ****************

(define non-simple?
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) #f)
      (var-exp (id) #f)
      (primapp-exp (prim rands)
        (exists? non-simple? rands))
      (if-exp (test-exp true-exp false-exp)
        (or
          (non-simple? test-exp)
          (non-simple? true-exp)
          (non-simple? false-exp)))
      (let-exp (ids rands body)
        (or
          (exists? non-simple? rands)
          (non-simple? body)))
      (proc-exp (ids body) #f)
      (app-exp (rator rands) #t)
      (letrec-exp (proc-names idss bodies letrec-body)
        (non-simple? letrec-body))
      (letcc-exp (id body) #t)
      (throw-exp (value-exp cont-exp) #t)
      )))

; (define simple?
;   (lambda (exp)
;     (not (non-simple? exp))))
