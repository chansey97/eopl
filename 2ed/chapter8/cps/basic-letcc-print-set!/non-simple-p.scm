;;; **************** syntactic stuff ****************

(define non-simple?
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) #f)
      (var-exp (id) #t)
      (genvar-exp (id) #f)
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
      (print-exp (exp) #t)
      (varassign-exp (id exp) #t)
      (letcc-exp (id body) #t)
      (throw-exp (value-exp cont-exp) #t)
      (else (eopl:error 'non-simple?
              "shouldn't call non-simple? on non-source expression ~s"
              exp))
      )))
