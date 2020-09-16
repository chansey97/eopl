#lang racket
(require "../parse.rkt")
(require "../cps.rkt")
(require "../final-valcont.rkt")

(define fact-iter-acc
  (λ (n a)
    (if (zero? n) a (fact-iter-acc (- n 1) (* n a)))))

(fact-iter-acc 5 1)

(unparse (cps-exp (parse '(λ (n a)
                            (if (zero? n) a (fact-iter-acc (- n 1) (* n a)))))))

(define fact-iter-acc-cps
  (λ (n a k)
    (if (zero? n)
        (k a)
        (fact-iter-acc-cps (- n 1) (* n a) k))))

(fact-iter-acc-cps 5 1 final-valcont)
