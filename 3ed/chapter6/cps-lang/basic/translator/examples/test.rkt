#lang eopl
(require "../../cps-in-lang.rkt")
(require "../cps.rkt")
(require racket/pretty)

(define (run-cps string)
  (pretty-print (cps-of-program (scan&parse string))))

(display (run-cps "(proc(x) -(x,1)  30)"))


