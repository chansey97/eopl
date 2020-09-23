;;; 8-5print.scm - conversion to cps with print and letcc

(let ((time-stamp "Time-stamp: <2000-12-19 17:02:58 wand>"))
  (eopl:printf "8-5print.scm - convert to cps with print and letcc ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define cps
  (lambda (string)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (scan&parse string)))))

(define cps-all
  (lambda ()
    (run-experiment cps use-execution-outcome
      '(lang8-4 lang8-5print) all-tests)))

(define cps-one
  (lambda (test-name)
    (run-test cps test-name)))

(define equal-external-reps? equal?)

