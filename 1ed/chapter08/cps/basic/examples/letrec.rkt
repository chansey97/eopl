#lang racket
(require "../parse.rkt")
(require "../cps.rkt")
(require "../final-valcont.rkt")

(unparse (cps-exp (parse '(letrec ((x 3)
                                   (y (λ () (+ x 1))))
                            (y)))))

;; '(letrec ((x 3)
;;           (y (λ (k) (k (+ x 1)))))
;;    (y k))


(unparse (cps-exp (parse '(letrec ((loop (λ (x)
                                           (+ 1 (loop x)))))
                            (loop 0)))))

;; '(letrec ((loop (λ (x k)
;;                   (loop x (λ (:loop1)
;;                             (k (+ 1 :loop1)))))))
;;    (loop 0 k))


;; Note:
;; This cps is wrong in lexical scoping (although it is ok, in dynamic scoping),
;; because `loop` now is a free variable
;; Thus, current cps implementation of letrec, the decl of letrec must be a proc, see p.255  
(unparse (cps-exp (parse '(letrec ((loop ((λ (x) x)
                                          (λ (x)
                                            (+ 1 (loop x))))))
                            (loop 0)))))

'((λ (x k) (k x))
  (λ (x k) (loop x (λ (:loop1)
                     (k (+ 1 :loop1)))))
  (λ (:g2) (letrec ((loop :g2))
             (loop 0 k))))


;; Name clash!
;; The outer's loop is different from the inner's loop
;; Even in dynamic scoping, it is still wrong...
;; I guess to make it works in dynamic scoping, we should not alpha-convert let bindings.
(unparse (cps-exp (parse '(loop (letrec ((loop ((λ (x) x)
                                                (λ (x)
                                                  (+ 1 (loop x))))))
                                  (loop 0))))))

'((λ (x k) (k x))
  (λ (x k) (loop x (λ (:loop4)
                     (k (+ 1 :loop4)))))
  (λ (:g5) (letrec ((loop: :g5))
             (loop: 0 (λ (:loop:6)
                        (loop :loop:6 k))))))


;; TODO:
;; Implement letrec like p.163, before that we must implement set! and begin first.
