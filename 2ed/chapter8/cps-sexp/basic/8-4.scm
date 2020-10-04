;;; 8-4.scm - conversion to cps

(let ((time-stamp "Time-stamp: <2000-12-21 15:38:36 wand>"))
  (eopl:printf "8-4.scm - convert to cps ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define cps
  (lambda (datum)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (parse datum)))))

