(define list-index                      ; in srfi-1
  (lambda (pred ls)
    (cond 
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((ans (list-index pred (cdr ls))))
              (if (number? ans) (+ 1 ans) #f))))))

(define list-set                        ; definitely ours!
  (lambda (ls index new)
    (if (zero? index)
      (cons new (cdr ls))
      (cons (car ls) (list-set (cdr ls) (- index 1) new)))))

(define exists?                         ; srfi-1 calls this ANY
  (lambda (pred lv)
    (cond
      ((null? lv) #f)
      ((pred (car lv)) #t)
      (else (exists? pred (cdr lv))))))

;;; **************** even more misc stuff ****************

;; supplied in sllgen
; (define list-of
;   (lambda (pred)
;     (lambda (ls)
;       (and (list? ls) (andmap pred ls)))))


;; **************** stuff from srfi-3 ****************

(define intersection                    ; srfi-3 calls this intersectionq
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (cons (car set1) (intersection (cdr set1) set2)))
      (else (intersection (cdr set1) set2)))))

(define union                           ; srfi-3 calls this unionq
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((memv (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define setdiff                         ; srfi-3 calls this
                                        ; list-difference(q,v) 
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2) (setdiff (cdr set1) set2) )
      (else (cons (car set1) (setdiff (cdr set1) set2))))))

(define mapunion                        ; (foldr union '() sets)
  (lambda (f sets)
    (cond
      ((null? sets) '())
      (else (union (f (car sets)) (mapunion f (cdr sets)))))))
