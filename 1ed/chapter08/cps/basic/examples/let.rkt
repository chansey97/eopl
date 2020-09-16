#lang racket
(require "../parse.rkt")
(require "../cps.rkt")
(require "../final-valcont.rkt")

(unparse (cps-exp (parse '(let ((x (f 1 1))
                                (y (g 2 2))
                                (z (h 3 3)))
                            (x y z)))))

;; '(f 1 1 (λ (:f1)
;;           (g 2 2 (λ (:g2)
;;                    (h 3 3 (λ (:h3)
;;                             (let ((x :f1)
;;                                   (y :g2)
;;                                   (z :h3))
;;                               (x y z k))))))))
