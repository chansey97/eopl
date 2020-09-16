#lang racket
(require "../parse.rkt")
(require "../cps.rkt")
(require "../final-valcont.rkt")

;;; Anonymous figure : page 272

;; (define remove
;;   (lambda (s los)
;;     (if (null? los)
;;         '()
;;         (if (eq? s (car los))
;;             (remove s (cdr los))
;;             (cons (car los) (remove s (cdr los)))))))

(unparse (cps-exp (parse '(lambda (s los)
                            (if (null? los)
                                '()
                                (if (eq? s (car los))
                                    (remove s (cdr los))
                                    (cons (car los) (remove s (cdr los)))))))))

;;; Anonymous figure : page 274

;; (define subst
;;   (lambda (new old slst)
;;     (if (null? slst)
;;         '()
;;         (if (symbol? (car slst))
;;             (if (eq? (car slst) old)
;;                 (cons new (subst new old (cdr slst)))
;;                 (cons (car slst) (subst new old (cdr slst))))
;;             (cons (subst new old (car slst))
;;                   (subst new old (cdr slst)))))))

(unparse (cps-exp (parse '(lambda (new old slst)
                            (if (null? slst)
                                '()
                                (if (symbol? (car slst))
                                    (if (eq? (car slst) old)
                                        (cons new (subst new old (cdr slst)))
                                        (cons (car slst) (subst new old (cdr slst))))
                                    (cons (subst new old (car slst))
                                      (subst new old (cdr slst)))))))))

;;; Anonymous figure : page 277

;; (define remove
;;   (lambda (s los)
;;     (letrec
;;       ((loop 
;;          (lambda (los)
;;            (if (null? los)
;;                '()
;;                (if (eq? s (car los))
;;                    (loop (cdr los))
;;                    (cons (car los) (loop (cdr los))))))))
;;       (loop los))))

(unparse (cps-exp (parse '(lambda (s los)
                            (letrec
                                ((loop 
                                  (lambda (los)
                                    (if (null? los)
                                        '()
                                        (if (eq? s (car los))
                                            (loop (cdr los))
                                            (cons (car los) (loop (cdr los))))))))
                              (loop los))))))
