#lang racket


(match '(define proc-name x y z)
  [`(define proc-name . ,xs) (displayln xs)])

