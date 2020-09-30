#lang eopl
(require "../../cps-in-lang.rkt")
(require "../../cps-out-lang.rkt")
(require "../cps.rkt")
(require racket/pretty)

;; (define (run-cps string)
;;   (pretty-print (cps-of-program (scan&parse string))))

;; (display (run-cps "((lambda (x) (- x 1)) 30)"))


;; (display (run-cps "x"))

;; (read "((lambda (x) (- x 1)) 30)")

(unparse (cps-of-program (parse '((lambda (x) (- x 1)) 30))))
