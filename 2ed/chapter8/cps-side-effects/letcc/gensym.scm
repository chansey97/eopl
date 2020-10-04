;;; **************** gensym ****************

;;; need this here for init-k (sorry)

(define gensymbol
  (let ((n 0))
    (lambda (s)
      (set! n (+ n 1))
      (let ((s (if (string? s) s (symbol->string s))))
        (string->symbol
          (string-append s (number->string n)))))))

