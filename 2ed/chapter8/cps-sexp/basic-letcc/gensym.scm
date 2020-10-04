;;; **************** gensym ****************

;;; need this here for init-k (sorry)

(define gensymbol
  (let ((n 0))
    (lambda (s)
      (set! n (+ n 1))
      (let ((s (if (string? s) s (symbol->string s))))
        (string->symbol
          (string-append s (number->string n)))))))

;; 8-5.scm from the original code seems missing the following code.
;; So copy from 8-4.scm.

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
