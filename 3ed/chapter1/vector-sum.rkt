#lang racket

;; If we were summing the values in a list, we could follow the grammar to recur on the cdr of the list. 
(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))

;; But it is not possible to proceed in this way with vectors, because they do not
;; decompose as readily.

;; Note: Instead of induced on list, we induced on n.

(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1))))))

(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n)
          0
          (partial-vector-sum v (- n 1))))))
