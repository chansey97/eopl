; EOPL exercise 1.32
;
; Write a procedure double-tree that takes a bintree, as represented in
; definition 1.1.7, and produces another bintree like the original, but with
; all the integers in the leaves doubled.

; We base this on the previous exercise

;; Definition 1.1.7 (binary tree)
;; Bintree ::= Int | (Symbol Bintree Bintree)

#lang racket
(require eopl)
(require "./bintree.rkt")

(define (double x)
  (+ x x))

(define (double-tree tree)
  (if (leaf? tree)
      (leaf (double (contents-of tree)))
      (interior-node (contents-of tree)
                     (double-tree (lson tree))
                     (double-tree (rson tree)))))

(interior-node 'root
               (interior-node 'left (leaf 1) (leaf 2))
               (interior-node 'right
                              (leaf 3)
                              (interior-node 'right2 (leaf 4) (leaf 5))))
(double-tree
 (interior-node 'root
                (interior-node 'left (leaf 1) (leaf 2))
                (interior-node 'right
                               (leaf 3)
                               (interior-node 'right2 (leaf 4) (leaf 5)))))
