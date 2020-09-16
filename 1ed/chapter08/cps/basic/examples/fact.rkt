#lang racket
(require "../parse.rkt")
(require "../cps.rkt")
(require "../final-valcont.rkt")

(define fact
  (λ (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))

(fact 5)

(unparse (cps-exp (parse '(λ (n)
                            (if (zero? n)
                                1
                                (* n (fact (- n 1))))))))

(define fact-cps
  (λ (n k)
    (if (zero? n)
        (k 1)
        (fact-cps (- n 1) (λ (:fact1)
                            (k (* n :fact1)))))))

(fact-cps 5 final-valcont)
