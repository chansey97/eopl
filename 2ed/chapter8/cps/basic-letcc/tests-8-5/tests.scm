(load "../../../../poor-man-module.scm")
(load-module "../../../../libs/" "r5rs")
(load-module "../../../../libs/" "sllgen")
(load-module "../../../../libs/" "define-datatype")
(load-module "../../../../libs/" "test-harness")

(load-module "../../" "basic-letcc")

;;; **************** tests ****************


(define pgm1  "letrec
  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
in (fact 6)")

(define pgm2  "letrec
         even(x) = if zero?(x) then 1 else (odd sub1(x))
         odd(x)  = if zero?(x) then 0 else (even sub1(x))
       in (odd 13)")

(define pgm3
  "letrec even(odd,x) =  if zero?(x) then 1 else (odd sub1(x))
   in letrec  odd(x)  = if zero?(x) then 0 else (even odd sub1(x))
   in (odd 13)")

(define pgm4 "+(1, letcc j in +(4, throw 3 to j))")

(define pgm5 "add1(if (f zero?(x)) then 1 else *(x, (fact sub1(x))))")

(define pgm6
  '(let ((a (f x)) (b (g x)) (c (h (i (j (t x))))))
     (cons a (cons b (cons c (empty))))))

(define pgm6 "
let a = (f x) b = (g x) c = (h (i (j (t x))))
in cons(a,cons(b,cons(c,emptylist())))")

;; curried map, using poor-man's Y
(define pgm7
  '(proc (f)
     (let ((foo (proc (foo ls)
                  (if (null? ls)
                    (empty)
                    (cons (f (car ls)) (foo foo (cdr ls)))))))
       (proc (ls) (foo foo ls)))))

(define pgm7 "
proc(f) let foo = proc(foo,ls) 
                   if null?(ls)
                   then emptylist()
                   else cons((f car(ls)), (foo foo cdr(ls)))
        in proc(ls)(foo foo ls)")

; a let that tries to capture a variable from the continuation.
(define pgm8
  '(f (let ((f 3)) (+ f 1))))

(define pgm8 "
 (f let f = 3 in add1(f))")

(define pgm9
  '(k (g (let ((g 3)) (f g x)))))

(define pgm9 "(k (g let g=3 in (f g x)))")

(define pgm10
  '(k (g (letcc g (f (g x))))))

(define pgm10 "let x = 4 g = proc(n)*(n,n) in (g letcc g in (f throw x to g))")

(define pgm11
  '(even
     (letrec
       ((even (x) (if (zero? x) 1 (odd (sub1 x))))
        (odd (x) (if (zero?) 0 (even (sub1 x)))))
       (odd 13))))

(define pgm11 "  (even
  letrec
    even(x) = if zero?(x) then 1 else (odd sub1(x))
    odd(x)  = if zero?(x) then 0 else (even sub1(x))
  in (odd 13))" )

(define test-all
  (lambda ()
    (for-each
      (lambda (pgm)
        (eopl:pretty-print pgm)
        (newline)
        (eopl:pretty-print (run pgm))
        (newline))
      (list pgm1 pgm2 pgm3 pgm4 pgm5 pgm6 pgm7 pgm8 pgm9 pgm11))))

;; pgm6 pgm7 pgm8 pgm9 pgm10 pgm11))))

(test-all)
