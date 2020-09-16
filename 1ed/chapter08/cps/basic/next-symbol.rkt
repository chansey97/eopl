#lang racket
(require "./ast.rkt")
(provide (all-defined-out))

;;; Figure 8.6.3 : page 283

(define next-symbol-right
  (lambda (sym)
    (string->symbol 
      (string-append (symbol->string sym) ":"))))

(define next-symbol-left
  (let ((c 0))
    (lambda (rator)
      (set! c (+ c 1))
      (string->symbol
        (string-append ":"
          (if (varref? rator) 
              (symbol->string (varref->var rator))
              "g")
          (number->string c))))))
