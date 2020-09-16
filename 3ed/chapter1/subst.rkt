#lang racket
;; The decomposition of subst into two procedures, one for each syntactic
;; category, is an important technique. It allows us to think about one syntactic
;; category at a time, which greatly simplifies our thinking about more
;; complicated programs.

;; S-list ::= ()
;;        ::= (S-exp . S-list)
;; S-exp ::= Symbol | S-list

;; subst : Sym × Sym × S-list → S-list
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (subst-in-s-exp new old (car slist))
         (subst new old (cdr slist))))))

;; subst-in-s-exp : Sym × Sym × S-exp → S-exp
(define subst-in-s-exp
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))

(subst 'a 'b '((b c) (b () d)))
