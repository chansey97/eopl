(define var-exp?
  (lambda (x)
    (cases expression x
      (var-exp (id) #t)
      (else #f))))
