;;; 8-5print.scm - conversion to cps with print and letcc

(let ((time-stamp "Time-stamp: <2000-12-19 17:02:58 wand>"))
  (eopl:printf "8-5print.scm - convert to cps with print and letcc ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define cps
  (lambda (datum)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (parse datum)))))
