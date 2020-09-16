#lang racket

;;; Anonymous figure : page 274

(define subst
  (lambda (new old slst)
    (if (null? slst)
        '()
        (if (symbol? (car slst))
            (if (eq? (car slst) old)
                (cons new (subst new old (cdr slst)))
                (cons (car slst) (subst new old (cdr slst))))
            (cons (subst new old (car slst))
                  (subst new old (cdr slst)))))))
