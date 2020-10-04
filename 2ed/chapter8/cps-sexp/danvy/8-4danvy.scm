;;; 8-4danvy.scm - one-pass conversion to cps

(let ((time-stamp "Time-stamp: <2000-12-19 15:40:30 wand>"))
  (eopl:printf "8-4danvy.scm - convert to cps in one pass ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define cps
  (lambda (datum)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (parse datum)))))

