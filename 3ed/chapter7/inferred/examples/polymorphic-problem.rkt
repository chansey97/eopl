#lang racket

(require "../top.scm")

(check "
  let f = proc (x : ?) x
  in if zero?(0)
    then (f 11)
    else (f zero?(0))
")

(run "
  let f = proc (x : ?) x
  in if zero?(0)
    then (f 11)
    else (f zero?(0))
")

;; see Exercise 7.28
