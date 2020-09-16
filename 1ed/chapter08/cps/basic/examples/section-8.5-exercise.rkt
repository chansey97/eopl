#lang racket
(require "../parse.rkt")
(require "../cps.rkt")
(require "../final-valcont.rkt")

;;; Exercise 8.5.3 : page 279, 280

;; Sorry, I haven't implemented `cond` yet

;; (define remove*
;;   (lambda (a alst)
;;     (cond
;;       ((null? alst) '())
;;       ((pair? (car alst))
;;        (cons (remove* a (car alst)) (remove* a (cdr alst))))
;;       ((eq? (car alst) a) (remove* a (cdr alst)))
;;       (else (cons (car alst) (remove* a (cdr alst)))))))

;; (define member*
;;   (lambda (a alst)
;;     (cond
;;       ((null? alst) #f)
;;       ((pair? (car alst))
;;        (or (member* a (car alst)) (member* a (cdr alst))))
;;       ((eq? (car alst) a) alst)
;;       (else (member* a (cdr alst))))))

;; (define remfirst*
;;   (lambda (a alst)
;;     (letrec ((loop (lambda (alst)
;;                      (cond
;;                        ((null? alst) '())
;;                        ((not (pair? (car alst)))
;;                        (if (eq? (car alst) a)
;;                            (cdr alst)
;;                            (cons (car alst) (loop (cdr alst)))))
;;                    ((equal? (loop (car alst)) (car alst))
;;                     (cons (car alst) (loop (cdr alst))))
;;                    (else (cons (loop (car alst)) (cdr alst)))))))
;;       (loop alst))))

;; (define depth
;;   (lambda (alst)
;;     (cond
;;       ((null? alst) 1)
;;       ((not (pair? (car alst))) (depth (cdr alst)))
;;       ((< (+ (depth (car alst)) 1) (depth (cdr alst)))
;;        (depth (cdr alst)))
;;       (else (+ (depth (car alst)) 1)))))

(define depth-with-let
  (lambda (alst)
    (if (null? alst)
        1
        (let ((drest (depth-with-let (cdr alst))))
          (if (pair? (car alst))
              (let ((dfirst (+ (depth-with-let (car alst)) 1)))
                (if (< dfirst drest) drest dfirst))
              drest)))))

(depth-with-let '(1 (4 5) 2 3))

(unparse (cps-exp (parse '(lambda (alst)
                            (if (null? alst)
                                1
                                (let ((drest (depth-with-let (cdr alst))))
                                  (if (pair? (car alst))
                                      (let ((dfirst (+ (depth-with-let (car alst)) 1)))
                                        (if (< dfirst drest) drest dfirst))
                                      drest)))))))

(define depth-with-let-cps
  (λ (alst k)
    (if (null? alst)
        (k 1)
        (depth-with-let-cps
         (cdr alst)
         (λ (:depth-with-let1)
           (let ((drest :depth-with-let1))
             (if (pair? (car alst))
                 (depth-with-let-cps
                  (car alst)
                  (λ (:depth-with-let2)
                    (k
                     (let ((dfirst (+ :depth-with-let2 1)))
                       (if (< dfirst drest) drest dfirst)))))
                 (k drest))))))))

(depth-with-let-cps '(1 (4 5) 2 3) final-valcont)
