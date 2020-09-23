;;; **************** cps transformer ****************

(define k-id (gensymbol "k"))

(define k-var-exp (var-exp k-id))

(define cps-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) 
        (proc-exp (list k-id)
          (cps-of-expression exp k-var-exp))))))

(define cps-of-simple-exp ;^ translation for simple expressions
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) (lit-exp datum))
      (var-exp (id) (var-exp id))
      (primapp-exp (prim rands) ;^ rands are known to be simple
        (primapp-exp prim (map cps-of-simple-exp rands)))
      (if-exp (test-exp true-exp false-exp)
        (if-exp
          (cps-of-simple-exp test-exp)
          (cps-of-simple-exp true-exp)
          (cps-of-simple-exp false-exp)))
      (let-exp (ids rands body)
        (let-exp ids
          (map cps-of-simple-exp rands)
          (cps-of-simple-exp body)))
      (proc-exp (ids body)
        (proc-exp
          (append ids (list k-id))
          (cps-of-expression body k-var-exp)))
      (letrec-exp (proc-names idss bodies letrec-body)
        (letrec-exp proc-names
          (map
            (lambda (ids)
              (append ids (list k-id)))
            idss)
          (map
            (lambda (body)
              (cps-of-expression body k-var-exp))
            bodies)
          (cps-of-simple-exp letrec-body)))
      (app-exp (rator rands)
        (eopl:error 'cps-of-simple-exp
          "Can't call on application ~s" exp))
      (letcc-exp (id body)
        (eopl:error 'cps-of-simple-exp
          "Can't call on letcc ~s"
          exp))
      (throw-exp (value cont)
        (eopl:error 'cps-of-simple-exp
          "Can't call on throw ~s"
          exp))
      )))

(define cps-of-expression 
  (lambda (exp k)
    (if (non-simple? exp)
      (cases expression exp
        (if-exp (test-exp true-exp false-exp)
          (cps-of-if-exp test-exp true-exp false-exp k))
        (primapp-exp (prim rands)
          (cps-of-primapp-exp prim rands k))
        (let-exp (ids rands body)
          (cps-of-let-exp ids rands body k))
        (app-exp (rator rands)
          (cps-of-app-exp rator rands k))
        (letrec-exp (proc-names idss bodies letrec-body)
          (cps-of-letrec-exp
            proc-names idss bodies letrec-body k))
        (letcc-exp (id body) (cps-of-letcc-exp id body k))
        (throw-exp (value-exp cont-exp)
          (cps-of-throw-exp value-exp cont-exp k))
        (else
          (eopl:error 'cps-of-expression
            "Can't call on  ~s" exp))
        )
      (csimple exp k))))

(define csimple
  (lambda (exp k)
    (cases expression k
      (proc-exp (ids body)
        (let-exp ids (list (cps-of-simple-exp exp)) body))
      (else (app-exp k (list (cps-of-simple-exp exp)))))))

(define cps-of-if-exp
  (lambda (test-exp true-exp false-exp k)
    (if (non-simple? test-exp)
      (let ((v-id (gensymbol "v")))
        (cps-of-expression test-exp
          (proc-exp (list v-id)
            (cps-of-expression 
              (if-exp (var-exp v-id) true-exp false-exp)
              k))))
      (if-exp (cps-of-simple-exp test-exp)
        (cps-of-expression true-exp k)
        (cps-of-expression false-exp k)))))

(define cps-of-primapp-exp
  (lambda (prim rands k)
    (let ((pos (list-index non-simple? rands))
          (v-id (gensymbol "v")))
            ;^; this always succeeds, because the expression is known to be
            ;^; non-simple. 
      (cps-of-expression (list-ref rands pos)
        (proc-exp (list v-id)
          (cps-of-expression
            (primapp-exp prim
              (list-set rands pos (var-exp v-id)))
            k))))))

(define cps-of-app-exp
  (lambda (rator rands k)
    (if (non-simple? rator)
      (let ((v-id (gensymbol "v")))
        (cps-of-expression rator
          (proc-exp (list v-id)
            (cps-of-expression
              (app-exp (var-exp v-id) rands)
              k))))
      (cps-of-app-exp-simple-rator rator rands k))))

(define cps-of-app-exp-simple-rator
  (lambda (rator rands k)
    (let ((pos (list-index non-simple? rands)))
      (if (number? pos)
        (let ((v-id (gensymbol "v")))
          (cps-of-expression
            (list-ref rands pos)
            (proc-exp (list v-id)
              (cps-of-expression
                (app-exp rator
                  (list-set rands pos (var-exp v-id)))
                k))))
        (app-exp (cps-of-simple-exp rator)
          (append
            (map cps-of-simple-exp rands)
            (list k)))))))

(define cbindk
  (lambda (exp k)
    (let-exp (list k-id) (list k)
      (cps-of-expression exp k-var-exp))))

(define cps-of-letrec-exp
  (lambda (proc-names idss bodies letrec-body k)
    (if (var-exp? k)
      (letrec-exp
        proc-names
        (map
          (lambda (ids)
            (append ids (list k-id)))
          idss)
        (map
          (lambda (body)
            (cps-of-expression body k-var-exp))
          bodies)
        (cps-of-expression letrec-body k))
      (cbindk
        (letrec-exp proc-names idss bodies letrec-body)
        k))))

(define cps-of-let-exp
  (lambda (ids rands body k)
    (if (var-exp? k)
      (let ((pos (list-index non-simple? rands)))
        (if (number? pos)
          (let ((z-id (gensymbol "z")))
            (cps-of-expression
              (list-ref rands pos)
              (proc-exp (list z-id)
                (cps-of-expression
                  (let-exp ids
                    (list-set rands pos (var-exp z-id))
                    body)
                  k))))
          (let-exp ids (map cps-of-simple-exp rands)
            (cps-of-expression body k))))
      (cbindk (let-exp ids rands body) k))))

(define cps-of-throw-exp
  (lambda (value-exp cont-exp k)
    (if (non-simple? cont-exp)
      (cps-of-throw-non-simple-value value-exp cont-exp k)
      (cps-of-throw-simple-value     value-exp cont-exp k))))

(define cps-of-throw-non-simple-value
  (lambda (value-exp cont-exp k)
    (let ((new-var (gensymbol "k")))
      (cps-of-expression value-exp
        (proc-exp (list new-var)
          (cps-of-expression 
            (throw-exp new-var cont-exp)
            k))))))

(define cps-of-throw-simple-value
  (lambda (value-exp cont-exp k)
    (if (non-simple? cont-exp)
      (cps-of-throw-simple-value-non-simple-cont value-exp cont-exp k)
      (app-exp
        (cps-of-simple-exp cont-exp)
        (list (cps-of-simple-exp value-exp))))))

(define cps-of-throw-simple-value-non-simple-cont
  (lambda (value-exp cont-exp k)
    (let ((new-var (gensymbol "v")))
      (cps-of-expression cont-exp
        (proc-exp (list new-var)
          (cps-of-expression
            (throw-exp value-exp new-var)
            k))))))

(define cps-of-letcc-exp
  (lambda (id body k)
    (if (var-exp? k)
      (let-exp (list id) (list k)
        (cps-of-expression body k))
      (cbindk (letcc-exp id body) k))))
