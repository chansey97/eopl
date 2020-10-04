;;; eight-5.scm - conversion to cps

(let ((time-stamp "Time-stamp: <2000-12-08 10:46:38 dfried>"))
  (eopl:printf "eight-4.scm - convert to cps ~a~%"
               (substring time-stamp 13 29)))

(define cps
  (lambda (datum)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (parse datum)))))
