;;; 8-5set.scm - conversion to cps with print, set, and letcc

(let ((time-stamp "Time-stamp: <2000-12-21 14:48:55 wand>"))
  (eopl:printf "8-5set.scm - convert to cps with set ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define cps
  (lambda (string)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (scan&parse string)))))

;;; This algorithm gives different output than the algorithm of 8-4,
;;; so the tests of lang8-4 are repeated in lang8-5set with the
;;; proper outcomes.

(define cps-all
  (lambda ()
    (run-experiment cps use-execution-outcome
      '(lang8-5set)        
      all-tests)))

(define cps-one
  (lambda (test-name)
    (run-test cps test-name)))

(define equal-external-reps? equal-up-to-gensyms?)  ; defined in
                                                    ; test-harness.scm
