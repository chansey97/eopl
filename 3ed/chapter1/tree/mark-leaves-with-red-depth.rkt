; EOPL exercise 1.33
;
; Write a procedure mark-leaves-with-red-depth that takes a bintree
; (definition 1.1.7), and produces a bintree of the same shape as the
; original, except that in the new tree, each leaf contains the integer of
; nodes between it and the root that contain the symbol red. For example, the
; expression
;
; (mark-leaves-with-red-depth
;   (interior-node 'red
;     (interior-node 'bar
;       (leaf 26)
;       (leaf 12))
;     (interior-node 'red
;       (leaf 11)
;       (interior-node 'quux
;         (leaf 117)
;         (leaf 14)))))
;
; which is written using the procedures defined in exercise 1.31, should
; return the bintree
;
; (red
;   (bar 1 1)
;   (red 2 (quux 2 2)))

;; Definition 1.1.7 (binary tree)
;; Bintree ::= Int | (Symbol Bintree Bintree)

#lang racket
(require eopl)
(require "./bintree.rkt")

(define (mark-leaves-with-red-depth tree)
  (mark-leaves-with-red-from tree 0))

(define (mark-leaves-with-red-from tree n)
  (if (leaf? tree)
      n
      (if (eqv? (contents-of tree) 'red)
          (interior-node 'red
                         (mark-leaves-with-red-from (lson tree) (+ n 1))
                         (mark-leaves-with-red-from (rson tree) (+ n 1)))
          (interior-node (contents-of tree)
                         (mark-leaves-with-red-from (lson tree) n)
                         (mark-leaves-with-red-from (rson tree) n)))))

(interior-node 'red
               (interior-node 'bar
                              (leaf 26)
                              (leaf 12))
               (interior-node 'red
                              (leaf 11)
                              (interior-node 'quux
                                             (leaf 117)
                                             (leaf 14))))

(mark-leaves-with-red-depth
 (interior-node 'red
                (interior-node 'bar
                               (leaf 26)
                               (leaf 12))
                (interior-node 'red
                               (leaf 11)
                               (interior-node 'quux
                                              (leaf 117)
                                              (leaf 14)))))
