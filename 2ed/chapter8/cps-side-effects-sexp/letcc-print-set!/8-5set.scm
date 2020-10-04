;;; 8-5set.scm - conversion to cps with print, set, and letcc

(let ((time-stamp "Time-stamp: <2000-12-21 14:48:55 wand>"))
  (eopl:printf "8-5set.scm - convert to cps with set ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define cps
  (lambda (datum)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (parse datum)))))
