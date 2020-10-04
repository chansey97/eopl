(load "../../poor-man-module.scm")
(load-module "../../" "r5rs" )
(load-module "../../" "sllgen")
(load-module "../../" "define-datatype")
(load-module "../../" "test-harness")

;;; ------------------------------
;;; examples

(define-datatype define-datatype:test:btree define-datatype:test:btree?
  (define-datatype:test:empty-btree)
  (define-datatype:test:btree-node
    (left define-datatype:test:btree?)
    (key integer?)
    (right define-datatype:test:btree?)))

(define sort-intlist
  (letrec ((flatten-btree
             (lambda (bt acc)
               (cases define-datatype:test:btree bt
                 (define-datatype:test:empty-btree () acc)
                 (define-datatype:test:btree-node (left key right)
                   (flatten-btree left
                     (cons key
                       (flatten-btree right acc)))))))
           (insert-list
             (lambda (ls bt)
               (if (null? ls) bt
                   (insert-list (cdr ls) (insert (car ls) bt)))))
           (insert
             (lambda (n bt)
               (cases define-datatype:test:btree bt
                 (define-datatype:test:empty-btree ()
                   (define-datatype:test:btree-node
                     (define-datatype:test:empty-btree)
                     n
                     (define-datatype:test:empty-btree)))
                 (define-datatype:test:btree-node (left key right)
                   (cond
                     ((equal? n key) bt)
                     ((< n key)
                      (define-datatype:test:btree-node (insert n left) key right))
                     (else
                       (define-datatype:test:btree-node left key (insert n right)))))))))
    (lambda (ls)
      (flatten-btree (insert-list ls (define-datatype:test:empty-btree)) '()))))

(define define-datatype:test0
  '(sort-intlist '(8 6 7 5 3 0 9)))

;;; ------------------------------

(define-datatype define-datatype:test:lyst define-datatype:test:lyst?
  (define-datatype:test:nil)
  (define-datatype:test:pair
    (head always?)
    (tail define-datatype:test:lyst?)))

(define list->lyst
  (lambda (ls)
    (if (null? ls)
        (define-datatype:test:nil)
        (define-datatype:test:pair (car ls) (list->lyst (cdr ls))))))

(define lyst->list                      ; this tests hygiene
  (lambda (pr)
    (cases define-datatype:test:lyst pr
      (define-datatype:test:nil () '())
      (define-datatype:test:pair (head tail)
        (cons head (lyst->list tail))))))

(define define-datatype:test1
  '(lyst->list (list->lyst '(this is a weird form of identity))))

(define lyst-nil?                       ; this tests else-ability
  (lambda (pr)
    (cases define-datatype:test:lyst pr
      (define-datatype:test:nil () #t)
      (else #f))))

(define define-datatype:test2
  '(list (lyst-nil? (define-datatype:test:nil)) (lyst-nil? (define-datatype:test:pair 3 (define-datatype:test:nil)))))

(define define-datatype:test3
  '(begin
     (define-datatype define-datatype:test:alist define-datatype:test:alist?
       (define-datatype:test:anil)
       (define-datatype:test:apair (head always?) (tail blist?)))
     (define-datatype define-datatype:test:blist define-datatype:test:blist?
       (define-datatype:test:bnil)
       (define-datatype:test:bpair (head always?) (tail define-datatype:test:alist?)))
     (define-datatype:test:apair 5 (define-datatype:test:bpair 4 (define-datatype:test:anil)))))

(define define-datatype:test4
  '(begin
     (define-datatype define-datatype:test:fff define-datatype:test:fff?
       (define-datatype:test:wu)
       (define-datatype:test:bu (define-datatype:test:fff define-datatype:test:fff?)))
     (let ((define-datatype:test:fff 3))
       (define-datatype:test:fff? (define-datatype:test:bu (define-datatype:test:wu))))))

;;; ------------------------------
;;; error tests

(define define-datatype:err0            ; wrong # args to a constructor
  '(define-datatype:test:pair))

(define define-datatype:err1            ; wrong type args to a constructor
  '(define-datatype:test:pair 3 4))

(define define-datatype:err2            ; unlisted variant in cases
  '(cases define-datatype:test:lyst (define-datatype:test:nil)
     (define-datatype:test:nil () 3)))

(define define-datatype:err3            ; wrong type argument to case
  '(cases define-datatype:test:lyst 5
     (define-datatype:test:pair (x y) 3)
     (define-datatype:test:nil () 8)))

(define define-datatype:err4            ; wrong # fields in cases
  '(cases define-datatype:test:lyst (define-datatype:test:nil)
     (define-datatype:test:pair (y z) 4)
     (define-datatype:test:nil (x) 3)
     (else 5)))

(define define-datatype:err5            ; misspelled variant in cases
  '(cases define-datatype:test:lyst (define-datatype:test:nil)
     (define-datatype:test:ppair (y z) 4)
     (define-datatype:test:nil () 8)
     (else 5)))

(define define-datatype:err10           ; non-symbol used for variant name
  '(define-datatype define-datatype:test:x define-datatype:test:x?
     ((define-datatype:test:r) (a b))))

(define define-datatype:err11           ; duplicate variant names
  '(define-datatype define-datatype:test:x define-datatype:test:x?
     (define-datatype:test:r (zoo goo?))
     (define-datatype:test:r (foo goo?))
     (define-datatype:test:s (joo goo?))))

(define define-datatype:err14           ; only type name
  '(define-datatype define-datatype:test:a define-datatype:test:a?))

(define define-datatype:err18           ; duplicate variant clauses
  '(cases define-datatype:test:lyst (define-datatype:test:nil)
     (define-datatype:test:nil () 3)
     (define-datatype:test:nil () 4)))

(define define-datatype:err19           ; repeated variant name.
  '(begin
     (define-datatype define-datatype:test:www define-datatype:test:www?
       (define-datatype:test:foo (man define-datatype:test:www?)))
     (define-datatype define-datatype:test:zzz define-datatype:test:zzz?
       (define-datatype:test:foo (harry define-datatype:test:zzz?)))))

;; uuu? is undefined.  Should it be uu?
(define define-datatype:err20           ; isa's symbol arg is not a type name
  '(begin
     (define-datatype define-datatype:test:uu define-datatype:test:uu?
       (define-datatype:test:goo (man number?)))
     (define-datatype:pretty-print (define-datatype:test:uu? (define-datatype:test:goo 5)))
     (define-datatype:test:uu? (define-datatype:test:goo 6))))

(define define-datatype:err21           ; Too many args to uuuu?
  '(begin
     (define-datatype define-datatype:test:uuuu define-datatype:test:uuuu?
       (define-datatype:test:gu)
       (define-datatype:test:zu (foo define-datatype:test:uuuu?)))
     (define-datatype:test:uuuu? (define-datatype:test:zu (define-datatype:test:zu (define-datatype:test:gu))) 5)))

;; what is the "?" on the last line??
(define define-datatype:err22           ; Too few args to isa
  '(begin
     (define-datatype define-datatype:test:uuuu define-datatype:test:uuuu?
       (define-datatype:test:gu)
       (define-datatype:test:zu (foo define-datatype:test:uuuu?)))
     ( (define-datatype:test:zu (define-datatype:test:zu (define-datatype:test:gu)))?)))

(define define-datatype:err23           ; Too many args to isa
  '(begin
     (define-datatype define-datatype:test:uuuu define-datatype:test:uuuu?
       (define-datatype:test:gu)
       (define-datatype:test:zu (foo define-datatype:test:uuuu?)))
     (uuuu? (define-datatype:test:zu (define-datatype:test:zu (define-datatype:test:gu))))))

(define define-datatype:err24           ; type name cannot be chosen
  '(begin                               ; from existing variant name
     (define-datatype define-datatype:test:uuuu define-datatype:test:uuuu?
       (define-datatype:test:gu)
       (define-datatype:test:zu (foo define-datatype:test:uuuu?)))
     (define-datatype define-datatype:test:gu define-datatype:test:gu?
       (hood))
     (define-datatype:test:uuuu? (define-datatype:test:zu (define-datatype:test:zu (define-datatype:test:gu))))))

(define define-datatype:err25           ; type name and constructor name
  '(begin                               ; cannot be the same.
     (define-datatype define-datatype:test:uuuu define-datatype:test:uuuu?
       (define-datatype:test:gu)
       (define-datatype:test:uuuu (foo define-datatype:test:uuuu?)))
     (define-datatype:test:uuuu? (define-datatype:test:zu (define-datatype:test:zu (define-datatype:test:gu))))))

(define define-datatype:err26           ; variantr name cannot be chosen
  '(begin                               ; from existing type name
     (define-datatype define-datatype:test:uuuu define-datatype:test:uuuu?
       (define-datatype:test:gu)
       (define-datatype:test:uuuu^ (foo define-datatype:test:uuuu?)))
     (define-datatype define-datatype:test:gru define-datatype:test:gru?
       (define-datatype:test:uuuu))
     (define-datatype:test:uuuu? (define-datatype:test:zu (define-datatype:test:zu (define-datatype:test:gu))))))



;; wdc: what is this 5 ?
(define define-datatype:err27           ; isa's arg is not a symbol.
  '(begin                               
     (define-datatype define-datatype:test:uuuu define-datatype:test:uuuu?
       (define-datatype:test:gu)
       (define-datatype:test:uuuu^ (foo define-datatype:test:uuuu?)))
     (5 (define-datatype:test:zu (define-datatype:test:zu (define-datatype:test:gu))))))



(define define-datatype:err28           ; 1st & 2nd arg of cases should not
  '(begin                               ; be the same.
     (define-datatype define-datatype:test:uuuu** define-datatype:test:uuuu**?
       (define-datatype:test:gud)
       (define-datatype:test:uuuu^^ (foo define-datatype:test:uuuu**?)))
     (let ((define-datatype:test:uuuu** (define-datatype:test:uuuu^^ (define-datatype:test:uuuu^^ (define-datatype:test:gud)))))
       (cases define-datatype:test:uuuu** define-datatype:test:uuuu**
         (define-datatype:test:gud () "Hello")
         (define-datatype:test:uuuu^^ (foo) "Goodbye")))))

(define define-datatype:err29           ; 2nd arg of cases should not
  '(begin                               ; be the same as any variant name.
     (define-datatype define-datatype:test:uuuu** define-datatype:test:uuuu**?
       (define-datatype:test:gud)
       (define-datatype:test:uuuu^^ (foo define-datatype:test:uuuu**?)))
     (let ((define-datatype:test:uuuu^^ (define-datatype:test:uuuu^^ (define-datatype:test:uuuu^^ (define-datatype:test:gud)))))
       (cases define-datatype:test:uuuu** define-datatype:test:uuuu^^
         (define-datatype:test:gud () "Hello")
         (define-datatype:test:uuuu^^ (foo) "Goodbye")))))

(define do-all-tests
  (let ((tests (list
                 define-datatype:test0
                 define-datatype:test1
                 define-datatype:test2
                 define-datatype:test3
                 define-datatype:test4  
                 define-datatype:err0
                 define-datatype:err1
                 define-datatype:err2
                 define-datatype:err3
                 define-datatype:err4
                 define-datatype:err5
                 define-datatype:err10
                 define-datatype:err11 
                 define-datatype:err14 
                 define-datatype:err18
                 define-datatype:err19
                 define-datatype:err21
                 define-datatype:err23 
                 define-datatype:err24
                 define-datatype:err25
                 define-datatype:err26
                 define-datatype:err28
                 define-datatype:err29))) 
    (lambda (chezer)
      (for-each chezer tests))))

; mw added dynamic-wind around rebinding of eopl:error-stop
(define define-datatype:tester
  (lambda (example)
    (display "------------------------------")
    (newline)
    (sllgen:pretty-print example)
    (display "-->")
    (newline)
    (call-with-current-continuation
     (lambda (k)
       (let ((alpha (lambda () (k #f))))
         (let ((swap (lambda ()
                     (let ((temp eopl:error-stop))
                       (set! eopl:error-stop alpha)
                       (set! alpha temp)))))
           (dynamic-wind
             swap
             (lambda () 
               (write (eval example (interaction-environment)))
               (newline)
               #t)
             swap)))))))

(define define-datatype:test-all
  (lambda ()
    (do-all-tests define-datatype:tester)
    (define-datatype:reset-registries)))


;; (define-datatype:test-all)

;; FIXME:
;; blist?: undefined;
;; This fails in original code as well.
