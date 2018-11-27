; EOPL exercise 1.28
;
; (merge loi1 loi2), where loi1 and loi2 are lists of integers that are sorted
; in ascending order, returns a sorted list of all integers in loi1 and loi2.
;
; > (merge '(1 4) '(1 2 8))
; (1 1 2 4 8)
; > (merge '(35 62 81 90 91) '(3 83 85 90))
; (3 35 62 81 83 85 90 90 91)

#lang racket

(define (merge loi1 loi2)
  (cond ((null? loi1) loi2)
        ((null? loi2) loi1)
        (else (let ((head1 (car loi1))
                    (head2 (car loi2)))
                (cond ((< head1 head2) (cons head1 (merge (cdr loi1) loi2)))
                      ((> head1 head2) (cons head2 (merge loi1 (cdr loi2))))
                      (else (cons head1 (cons head2 (merge (cdr loi1) (cdr loi2))))))))))

(merge '(1 4) '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))
