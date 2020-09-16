#lang racket
(provide (all-defined-out))

;;; Exercise 8.5.2 : page 279

(define final-valcont
  (lambda (v)
    (display "The answer is: ")
    (write v)
    (newline)))
