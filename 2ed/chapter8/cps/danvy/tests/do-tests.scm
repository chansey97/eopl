(load "../../../../poor-man-module.scm")
(load-module "../../../../libs/" "r5rs")
(load-module "../../../../libs/" "sllgen")
(load-module "../../../../libs/" "define-datatype")
(load-module "../../../../libs/" "test-harness")

(load-module "../../" "danvy")
(load "../../basic/tests/test-suite.scm")

(let ((time-stamp "Time-stamp: <2000-12-27 16:43:20 dfried>"))
  (display "EOPL test loader ")
  (display (substring time-stamp 13 29))
  (newline))

(define (test8-4) (cps-all))

(define do-all-tests
  (lambda ()
    (for-each 
      (lambda (p) (p))
      (list
        test8-4
        (lambda () (eopl:printf "no bugs found~%"))))))

(do-all-tests)
