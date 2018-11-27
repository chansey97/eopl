; EOPL exercise 2.01
;
; Implement the four required operations for bigits. Then use your
; implementation to calculate the factorial of 10. How does the execution time
; vary as this argument changes? How does the execution time vary as the base
; changes? Explain why.

; And this has only one star? Anyway. The results of execution time is in the
; end of the file.

#lang racket
(provide (all-defined-out))
(require eopl)

; Let's use Racket parameters to simplify testing and benchmarking:

(define base (make-parameter 10))

; Some observers that will be used:

(define (first-bigit bignum) (car bignum))
(define (rest-bigits bignum) (cdr bignum))

; The four required operations:

(define (zero)
  '())

(define (is-zero? bignum)
  (null? bignum))

(define (successor bignum)
  (cond ((null? bignum) '(1))
        ((eqv? (first-bigit bignum) (- (base) 1))
         (cons 0 (successor (rest-bigits bignum))))
        (else
         (cons (+ (first-bigit bignum) 1)
               (rest-bigits bignum)))))

(define (predecessor bignum)
  (cond ((null? bignum)
         (eopl:error 'predecessor "We don't support negative numbers"))
        ((equal? bignum '(1)) '())
        ((zero? (first-bigit bignum))
         (cons (- (base) 1)
               (predecessor (rest-bigits bignum))))
        (else
         (cons (- (first-bigit bignum) 1)
               (rest-bigits bignum)))))

; Converting a bignum to int:

(define (bignum->int bignum)
  (if (is-zero? bignum)
      0
      (+ (first-bigit bignum)
         (* (base) (bignum->int (rest-bigits bignum))))))

(define (int->bignum n)
  (define (iter n result)
    (if (zero? n)
        result
        (iter (- n 1) (successor result))))
  (iter n (zero)))

; Some additional operations in order to have a nicer factorial:

(define (add bignum1 bignum2)
  (if (is-zero? bignum1)
      bignum2
      (add (predecessor bignum1) (successor bignum2))))

;; iter version
(define (multiply bignum1 bignum2)
  (define (iter n result)
    (if (is-zero? n)
        result
        (iter (predecessor n) (add bignum1 result))))
  (iter bignum2 (zero)))

;; (define (multiply bignum1 bignum2)
;;   (cond ((is-zero? bignum1) (zero))
;;         ((is-zero? (predecessor bignum1)) bignum2)
;;         (else (add bignum2 (multiply (predecessor bignum1) bignum2))) ))
