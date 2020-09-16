; EOPL exercise 1.35
;
; Write a procedure number-leaves that takes a bintree, and produces a bintree
; like the original, except the contents of the leaves are numbered starting
; from 0. For example,
;
; (number-leaves
;   (interior-node 'foo
;     (interior-node 'bar
;       (leaf 26)
;       (leaf 12))
;     (interior-node 'baz
;       (leaf 11)
;       (interior-node 'quux
;         (leaf 117)
;         (leaf 14)))))
;
; should return
;
; (foo
;   (bar 0 1)
;   (baz
;     2
;     (quux 3 4)))

#lang racket
(require "./bintree.rkt")

(define (number-leaves tree)
  (car (number-leaves-from tree 0)))

(define (number-leaves-from tree n)
  (if (leaf? tree)
      (list (leaf n) (+ n 1))
      (let* ((left (number-leaves-from (lson tree) n))
             (left-tree (car left))
             (left-n (cadr left))
             (right (number-leaves-from (rson tree) left-n))
             (right-tree (car right))
             (right-n (cadr right))
             )
        (list (interior-node (contents-of tree)
                             left-tree
                             right-tree)
              right-n))
      ))

(number-leaves
 (interior-node 'foo
                (interior-node 'bar
                               (leaf 26)
                               (leaf 12))
                (interior-node 'baz
                               (leaf 11)
                               (interior-node 'quux
                                              (leaf 117)
                                              (leaf 14)))))
