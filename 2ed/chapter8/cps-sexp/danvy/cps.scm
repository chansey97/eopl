;;; **************** cps transformer ****************

(define k-id (gensymbol "k"))
(define k-var-exp (var-exp k-id))
(define new-ids
  (lambda (ids)
    (append ids (list k-id))))
(define var-exp?
  (lambda (exp)
    (cases expression exp
      (var-exp (id) #t)
      (else #f))))

(define possible-eta  
  (lambda (exp)
    (cases expression exp
      (proc-exp (ids body)
        (cases expression body
          (app-exp (rator rands)
            (if (singleton? rands)
              (cases expression (car rands)
                (var-exp (id)
                  (if (singleton? ids)
                    (if (eqv? (car ids) id) ;and not (occurs-free? id body)
                      rator
                      exp)
                    exp))
                (else exp))
              exp))
          (else exp)))
      (else exp))))

(define singleton?
  (lambda (ls)
    (and (not (null? ls))
         (null? (cdr ls)))))

(define non-simple?
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) #f)
      (var-exp (id) #f)
      (proc-exp (ids body) #f)
      (else #t))))

(define cps-of-program        
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
        (proc-exp (list k-id) (cps-of-tail-pos exp))))))

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
          (cps-of-letrec-exp proc-names idss bodies letrec-body k))
;         (letcc-exp (id body) (cps-of-letcc-exp id body k))
;         (throw-exp (value-exp cont-exp)
;           (cps-of-throw-exp value-exp cont-exp k))
        (else
          (eopl:error 'cps-of-expression
            "Can't call on  ~s" exp)))
      (csimple exp k)
      )))

(define cps-of-simple-exp 
  (lambda (exp)
    (cases expression exp
      (proc-exp (ids body)
        (proc-exp (new-ids ids)
          (cps-of-tail-pos body)))
      (lit-exp (datum) (lit-exp datum))
      (var-exp (id) (var-exp id))
      (else
        (error 'cps-of-simple-exp
          "Only procedures are simple:~s" exp)))))

;;; ***** new stuff ******
;------------------------------------------------------
(define csimple     
  (lambda (exp k)
    (k (cps-of-simple-exp exp))))

(define cps-of-tail-pos
  (lambda (exp)
    (cps-of-expression exp
      (lambda (res)
        (app-exp k-var-exp (list res))))))

(define cps-of-app-exp  
  (lambda (rator rands k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id)
                (k (var-exp v-id))))))
      (cps-of-expression rator
        (lambda (rator-res)
          (cps-of-rands rands
            (lambda (rands-res)
              (app-exp rator-res
                (append rands-res
                  (list cont-exp))))))))))

(define cps-of-rands    
  (lambda (rands k)
    (if (null? rands)
      (k '())
      (cps-of-expression (car rands)
        (lambda (rand-res)
          (cps-of-rands (cdr rands)
            (lambda (rands-res)
              (k (cons rand-res rands-res)))))))))

(define cps-of-let-exp  
  (lambda (ids rands body k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id)
                (k (var-exp v-id))))))
      (cbindk (cps-of-rands rands
                (lambda (rands-res)
                  (let-exp ids rands-res
                    (cps-of-tail-pos body))))
        cont-exp))))
;------------------------------------------------------------
(define cps-of-if-exp   
  (lambda (test-exp true-exp false-exp k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id)
                (k (var-exp v-id))))))
      (cbindk
        (cps-of-expression test-exp
          (lambda (test-exp-res)
            (if-exp test-exp-res
              (cps-of-tail-pos true-exp)
              (cps-of-tail-pos false-exp))))
        cont-exp))))

(define cps-of-primapp-exp
  (lambda (prim rands k)
    (cps-of-rands rands
      (lambda (rands-res)
        (k (primapp-exp prim rands-res))))))

(define cbindk
  (lambda (exp cont-exp)
    (let-exp (list k-id) (list cont-exp) exp)))

(define cps-of-letrec-exp 
  (lambda (proc-names idss bodies letrec-body k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id) (k (var-exp v-id))))))
      (cbindk
        (letrec-exp
          proc-names
          (map new-ids idss)
          (map cps-of-tail-pos bodies)
          (cps-of-tail-pos letrec-body)) cont-exp))))

(define cps-of-letcc-exp  
  (lambda (id body k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id) (k (var-exp v-id))))))
      (let ((exp (let-exp (list id) (list k-var-exp)
                   (cps-of-tail-pos body))))
        (cbindk exp cont-exp)))))

(define cps-of-throw-exp 
  (lambda (value-exp cont-exp k)
    (cps-of-expression value-exp
      (lambda (value-exp-res)
        (cps-of-expression cont-exp
          (lambda (cont-exp-res)
            (app-exp cont-exp-res (list value-exp-res))))))))
