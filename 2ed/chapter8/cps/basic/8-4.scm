;;; 8-4.scm - conversion to cps

(let ((time-stamp "Time-stamp: <2000-12-21 15:38:36 wand>"))
  (eopl:printf "8-4.scm - convert to cps ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define cps
  (lambda (string)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (scan&parse string)))))

;; The test suite checks that the right cps code is produced.
;; We unparse the answer back to a Scheme-like form so we can
;; check it with equal?.  Another approach would be to put
;; defined-language code in the test-suite, and have
;; equal-external-reps? parse it.

;; Still another experiment would be to run the cps'd code and make
;; sure it gets the right answers, in the style of chapter 6.  


(define cps-all
  (lambda ()
    (run-experiment cps use-execution-outcome
      '(lang8-4) all-tests)))

(define cps-one
  (lambda (test-name)
    (run-test cps test-name)))

(define equal-external-reps? equal-up-to-gensyms?)




    






