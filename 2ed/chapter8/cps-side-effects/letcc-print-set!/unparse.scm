;; unparse: convert target code back to Scheme for easy evaluation.

(define unparse  
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) id)
      (genvar-exp (id) id)
      (primapp-exp (prim rands)
        (cons (unparse-prim prim) (map unparse rands)))
      (if-exp (test-exp true-exp false-exp)
        (list 'if
          (unparse test-exp)
          (unparse true-exp)
          (unparse false-exp)))
      (let-exp (ids rands body)
        (list 'let (map
                     (lambda (id rand)
                       (list id (unparse rand)))
                     ids rands)
          (unparse body)))
      (proc-exp (ids body)
        (list 'lambda ids (unparse body)))
      (app-exp (rator rands)
        (cons (unparse rator) (map unparse rands)))
      (letrec-exp (proc-names idss bodies letrec-body)
        (list 'letrec
          (map
            (lambda (proc-name ids body)
              (list proc-name (list 'lambda ids (unparse body))))
            proc-names idss bodies)
          (unparse letrec-body)))
;      (print-exp (exp) (list 'write (unparse exp)))
      (print-exp (exp)
        (eopl:error 'unparse
          "print should not appear in target expression ~s"
          exp))
      (printc-exp (exp cont) (list 'printc (unparse exp) (unparse cont)))
      (letcc-exp (id body)
        (list 'letcc id (unparse body)))
      (throw-exp (value-exp cont-exp)
        (list 'throw (unparse value-exp) (unparse cont-exp)))
;      (varassign-exp (id exp) (list 'set! id (unparse exp)))
      (varassign-exp (id exp)
        (eopl:error 'unparse
          "varassign should not appear in target expression ~s"
          exp))
      (varassignc-exp (id exp cont)
        (list 'setc id (unparse exp) (unparse cont)))
      (derefc-exp (id cont)
        (list 'derefc id (unparse cont)))
      )))

(define unparse-prim
  (lambda (p)
    (cases primitive p
      (plus-prim () '+)
      (minus-prim () '-)
      (mult-prim () '*)
      (incr-prim () 'add1)
      (decr-prim () 'sub1)
      (zero?-prim () 'zero?)
      (car-prim () 'car)
      (cdr-prim () 'cdr)
      (cons-prim () 'cons)
      (null?-prim () 'null?)
      (empty-prim () 'empty)
      (list-prim () 'list)
      )))

;; for running the cps'd programs in Scheme
(define derefc (lambda (x k) (k x)))

(define-syntax setc
  (syntax-rules ()
    ((setc var val k)
     (begin (set! var val) (k (void))))))
