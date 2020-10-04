;;; **************** gensym ****************

;;; need this here for init-k (sorry)

(define gensymbol
  (let ((n 0))
    (lambda (s)
      (set! n (+ n 1))
      (let ((s (if (string? s) s (symbol->string s))))
        (string->symbol
          (string-append s (number->string n)))))))

;; actually, we lied:  we need to be able to reset the gensymbol
;; counter

(define initialize-gensymbol-counter! '*)
(define gensymbol '*)

(let ((n 0))
  (set! initialize-gensymbol-counter!
    (lambda () (set! n 0)))
  (set! gensymbol
    (lambda (s)
      (set! n (+ n 1))
      (let ((s (if (string? s) s (symbol->string s))))
        (string->symbol
          (string-append s (number->string n)))))))

(define genvar-exp?
  (lambda (x)
    (cases expression x
      (genvar-exp (id) #t)
      (else #f))))
