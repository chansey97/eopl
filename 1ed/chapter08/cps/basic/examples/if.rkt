#lang racket
(require "../parse.rkt")
(require "../cps.rkt")
(require "../final-valcont.rkt")

(unparse (cps-exp (parse '(+ 1 (* (if (null? ls) 0 (+ (length (cdr ls)) 1)) 3)))))

;; '(if (null? ls)
;;    (k (+ 1 (* 0 3)))
;;    (length (cdr ls) (λ (:length1)
;;                       (k (+ 1 (* (+ :length1 1) 3))))))

;; The current implementation duplicates context (k (+ 1 (* [] 3)))

;; TODO:
;; see p.267 and Exercise 8.4.8

