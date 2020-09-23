;;;;;;;;;;;;;;;; chapter 8: cps ;;;;;;;;;;;;;;;;

(add-test! 'lang8-4 'cps-1 "letrec
  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
in (fact 6)"
' (lambda (k1)
    (letrec ((fact
               (lambda (x k1)
                 (if (zero? x)
                   (k1 1)
                   (fact (sub1 x) (lambda (v1) (k1 (* x v1))))))))
      (fact 6 k1))))

(add-test! 'lang8-4 'cps-2 "letrec
         even(x) = if zero?(x) then 1 else (odd sub1(x))
         odd(x)  = if zero?(x) then 0 else (even sub1(x))
       in (odd 13)"
  '(lambda (k1)
     (letrec ((even
                (lambda (x k1) (if (zero? x) (k1 1) (odd (sub1 x) k1))))
              (odd
                (lambda (x k1) (if (zero? x) (k1 0) (even (sub1 x) k1)))))
       (odd 13 k1))))

(add-test! 'lang8-4 'cps-3 "
letrec even(odd,x) =  if zero?(x) then 1 else (odd sub1(x))
   in letrec  odd(x)  = if zero?(x) then 0 else (even odd sub1(x))
   in (odd 13)"
  '(lambda (k1)
     (letrec ((even
                (lambda (odd x k1) (if (zero? x) (k1 1) (odd (sub1 x) k1)))))
       (letrec ((odd
                  (lambda (x k1)
                    (if (zero? x) (k1 0) (even odd (sub1 x) k1)))))
         (odd 13 k1)))))

(add-test! 'lang8-4 'cps-4 "
add1(if (f zero?(x)) then 1 else *(x, (fact sub1(x))))"
  '(lambda (k1)
     (f (zero? x)
       (lambda (v2)
         (if v2
           (let ((v1 1)) (k1 (add1 v1)))
           (fact (sub1 x)
             (lambda (v3) (let ((v1 (* x v3))) (k1 (add1 v1))))))))))

(add-test! 'lang8-4 'cps-5 "
let a = (f x) b = (g x) c = (h (i (j (t x))))
in cons(a,cons(b,cons(c,emptylist())))"
  '(lambda (k1)
     (f x
       (lambda (z1)
         (g x
           (lambda (z2)
             (t x
               (lambda (v6)
                 (j v6
                   (lambda (v5)
                     (i v5
                       (lambda (v4)
                         (h v4
                           (lambda (z3)
                             (k1 (let ((a z1) (b z2) (c z3))
                                   (cons a
                                     (cons b
                                       (cons c
                                         (empty))))))))))))))))))))

(add-test! 'lang8-4 'cps-6 "
proc(f) let foo = proc(foo,ls) 
                   if null?(ls)
                   then emptylist()
                   else cons((f car(ls)), (foo foo cdr(ls)))
        in proc(ls)(foo foo ls)"
  '(lambda (k1)
     (k1 (lambda (f k1)
           (k1 (let ((foo
                       (lambda (foo ls k1)
                         (if (null? ls)
                           (k1 (empty))
                           (f (car ls)
                             (lambda (v1)
                               (foo foo
                                 (cdr ls)
                                 (lambda (v2) (k1 (cons v1 v2))))))))))
                 (lambda (ls k1) (foo foo ls k1))))))))

(add-test! 'lang8-4 'cps-7 "
 (f let f = 3 in add1(f))"
  '(lambda (k1) (f (let ((f 3)) (add1 f)) k1)))

(add-test! 'lang8-4 'cps-8 "(k (g let g=3 in (f g x)))"
  '(lambda (k1)
     (let ((k1 (lambda (v2) (g v2 (lambda (v1) (k v1 k1))))))
       (let ((g 3)) (f g x k1)))))

(add-test! 'lang8-4 'cps-8 "(even
  letrec
    even(x) = if zero?(x) then 1 else (odd sub1(x))
    odd(x)  = if zero?(x) then 0 else (even sub1(x))
  in (odd 13))"
  '(lambda (k1)
     (let ((k1 (lambda (v1) (even v1 k1))))
       (letrec ((even
                  (lambda (x k1) (if (zero? x) (k1 1) (odd (sub1 x) k1))))
                (odd
                  (lambda (x k1) (if (zero? x) (k1 0) (even (sub1 x) k1)))))
         (odd 13 k1)))))
