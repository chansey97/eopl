#lang eopl
(require "../../cps-in-lang.rkt")
(require "../../cps-out-lang.rkt")
(require "../../translator/cps.rkt")
(require "../interp.rkt")
(require racket/pretty)

(define run
  (lambda (string)
    (let ((cpsed-pgm
           (cps-of-program (scan&parse string))))
      (pretty-print cpsed-pgm)
      (value-of-program cpsed-pgm))))

(display (run "(proc(x) -(x,1)  30)"))
