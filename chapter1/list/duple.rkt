; EOPL exercise 1.15
;
; (duple n x) returns a list containing n copies of x.
;
; > (duple 2 3)
; (3 3)
; > (duple 4 '(ha ha))
; ((ha ha) (ha ha) (ha ha) (ha ha))
; > (duple 0 '(blah))
; '()

#lang racket

;; Deep copy
;; (define (duple n x)
;;   (cond ((= n 0) '())
;;         (else (cons (copy x) (duple (- n 1) x)))))

;; ;; base scheme-value grammar
;; (define (copy x)
;;   (cond ((not (pair? x)) x)
;;         (else (cons (copy (car x)) (copy (cdr x))))))

(define duple
  (lambda (count item)
    (if (zero? count)
      '()
(cons item (duple (- count 1) item)))))

(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))
