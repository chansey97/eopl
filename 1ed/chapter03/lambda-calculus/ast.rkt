#lang racket
(require "../../define-record.rkt")
(provide (all-defined-out))

;; 3.4.3 Abstract Syntax and its Representation Using Records

(define-record lit (datum))
(define-record varref (var))
(define-record lambda (formal body))
(define-record app (rator rand))
