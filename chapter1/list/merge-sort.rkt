; EOPL exercise 1.29
;
; (sort loi) returns a list of the elements of loi in ascending order.
;
; > (sort '(8 2 5 2 3))
; (2 2 3 5 8)

#lang racket

;; (define (merge loi1 loi2)
;;   (cond ((null? loi1) loi2)
;;         ((null? loi2) loi1)
;;         (else (let ((head1 (car loi1))
;;                     (head2 (car loi2)))
;;                 (cond ((< head1 head2) (cons head1 (merge (cdr loi1) loi2)))
;;                       ((> head1 head2) (cons head2 (merge loi1 (cdr loi2))))
;;                       (else (cons head1 (cons head2 (merge (cdr loi1) (cdr loi2))))))))))

;; only use 1 pred: <
(define (merge loi1 loi2)
  (cond ((null? loi1) loi2)
        ((null? loi2) loi1)
        (else (let ((head1 (car loi1))
                    (head2 (car loi2)))
                (cond ((< head1 head2) (cons head1 (merge (cdr loi1) loi2)))
                      (else (cons head2 (merge loi1 (cdr loi2)))))))))

(define (split loi)
  (define (iter first second n)
    (if (zero? n)
        (list first second)
        (iter (cdr first) (cons (car first) second) (- n 1))))
  (iter loi '() (quotient (length loi) 2)))

(define (merge-sort loi)
  (if (<= (length loi) 1)
      loi
      (let* ((lists (split loi))
             (first (car lists))
             (second (cadr lists)))
        (merge (merge-sort first)
               (merge-sort second)))))

(define (sort loi)
  (merge-sort loi))

(sort ’(8 2 5 2 3))
