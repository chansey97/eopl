;;; tests for 8-5set.scm.  These duplicate the tests in 8-4.scm, since
;;; they cps differently.

(add-test!
  'lang8-5set
  'pgm8-5-1
  "letrec
  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
in (fact 6)"
  '(lambda (k1)
     (letrec ((fact
               (lambda (x k1)
                 (derefc
                   x
                   (lambda (v7)
                     (let ((v1 (zero? v7)))
                       (if v1
                           (k1 1)
                           (derefc
                             x
                             (lambda (v2)
                               (derefc
                                 fact
                                 (lambda (v4)
                                   (derefc
                                     x
                                     (lambda (v6)
                                       (let ((v5 (sub1 v6)))
                                         (v4 v5
                                             (lambda (v3)
                                               (k1 (* v2
                                                      v3))))))))))))))))))
       (derefc fact (lambda (v8) (v8 6 k1))))))
(add-test!
  'lang8-5set
  'pgm8-5-2
  "letrec
         even(x) = if zero?(x) then 1 else (odd sub1(x))
         odd(x)  = if zero?(x) then 0 else (even sub1(x))
       in (odd 13)"
  '(lambda (k1)
     (letrec ((even
               (lambda (x k1)
                 (derefc
                   x
                   (lambda (v10)
                     (let ((v6 (zero? v10)))
                       (if v6
                           (k1 1)
                           (derefc
                             odd
                             (lambda (v7)
                               (derefc
                                 x
                                 (lambda (v9)
                                   (let ((v8 (sub1 v9)))
                                     (v7 v8 k1))))))))))))
              (odd
               (lambda (x k1)
                 (derefc
                   x
                   (lambda (v5)
                     (let ((v1 (zero? v5)))
                       (if v1
                           (k1 0)
                           (derefc
                             even
                             (lambda (v2)
                               (derefc
                                 x
                                 (lambda (v4)
                                   (let ((v3 (sub1 v4)))
                                     (v2 v3 k1)))))))))))))
       (derefc odd (lambda (v11) (v11 13 k1))))))
(add-test!
  'lang8-5set
  'pgm8-5-3
  "letrec even(odd,x) =  if zero?(x) then 1 else (odd sub1(x))
   in letrec  odd(x)  = if zero?(x) then 0 else (even odd sub1(x))
   in (odd 13)"
  '(lambda (k1)
     (letrec ((even
               (lambda (odd x k1)
                 (derefc
                   x
                   (lambda (v5)
                     (let ((v1 (zero? v5)))
                       (if v1
                           (k1 1)
                           (derefc
                             odd
                             (lambda (v2)
                               (derefc
                                 x
                                 (lambda (v4)
                                   (let ((v3 (sub1 v4)))
                                     (v2 v3 k1)))))))))))))
       (letrec ((odd
                 (lambda (x k1)
                   (derefc
                     x
                     (lambda (v11)
                       (let ((v6 (zero? v11)))
                         (if v6
                             (k1 0)
                             (derefc
                               even
                               (lambda (v7)
                                 (derefc
                                   odd
                                   (lambda (v8)
                                     (derefc
                                       x
                                       (lambda (v10)
                                         (let ((v9 (sub1 v10)))
                                           (v7 v8 v9 k1)))))))))))))))
         (derefc odd (lambda (v12) (v12 13 k1)))))))
(add-test!
  'lang8-5set
  'pgm8-5-4
  "+(1, letcc j in +(4, throw 3 to j))"
  '(lambda (k1)
     (let ((k1 (lambda (v1) (k1 (+ 1 v1)))))
       (let ((j k1)) (derefc j (lambda (v3) (v3 3)))))))
(add-test!
  'lang8-5set
  'pgm8-5-5
  "add1(if (f zero?(x)) then 1 else *(x, (fact sub1(x))))"
  '(lambda (k1)
     (derefc
       f
       (lambda (v8)
         (derefc
           x
           (lambda (v10)
             (let ((v9 (zero? v10)))
               (v8 v9
                   (lambda (v2)
                     (if v2
                         (let ((v1 1)) (k1 (add1 v1)))
                         (derefc
                           x
                           (lambda (v3)
                             (derefc
                               fact
                               (lambda (v5)
                                 (derefc
                                   x
                                   (lambda (v7)
                                     (let ((v6 (sub1 v7)))
                                       (v5 v6
                                           (lambda (v4)
                                             (let ((v1 (* v3 v4)))
                                               (k1 (add1 v1))))))))))))))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-6
  "
let a = (f x) b = (g x) c = (h (i (j (t x))))
in cons(a,cons(b,cons(c,emptylist())))"
  '(lambda (k1)
     (derefc
       f
       (lambda (v19)
         (derefc
           x
           (lambda (v20)
             (v19 v20
                  (lambda (z1)
                    (derefc
                      g
                      (lambda (v17)
                        (derefc
                          x
                          (lambda (v18)
                            (v17 v18
                                 (lambda (z2)
                                   (derefc
                                     h
                                     (lambda (v9)
                                       (derefc
                                         i
                                         (lambda (v11)
                                           (derefc
                                             j
                                             (lambda (v13)
                                               (derefc
                                                 t
                                                 (lambda (v15)
                                                   (derefc
                                                     x
                                                     (lambda (v16)
                                                       (v15 v16
                                                            (lambda (v14)
                                                              (v13 v14
                                                                   (lambda (v12)
                                                                     (v11 v12
                                                                          (lambda (v10)
                                                                            (v9 v10
                                                                                (lambda (z3)
                                                                                  (let ((a
                                                                                         z1)
                                                                                        (b
                                                                                         z2)
                                                                                        (c
                                                                                         z3))
                                                                                    (derefc
                                                                                      a
                                                                                      (lambda (v4)
                                                                                        (derefc
                                                                                          b
                                                                                          (lambda (v6)
                                                                                            (derefc
                                                                                              c
                                                                                              (lambda (v8)
                                                                                                (let ((v7
                                                                                                       (cons v8
                                                                                                             (empty))))
                                                                                                  (let ((v5
                                                                                                         (cons v6
                                                                                                               v7)))
                                                                                                    (k1 (cons v4
                                                                                                              v5)))))))))))))))))))))))))))))))))))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-7
  "
proc(f) let foo = proc(foo,ls) 
                   if null?(ls)
                   then emptylist()
                   else cons((f car(ls)), (foo foo cdr(ls)))
        in proc(ls)(foo foo ls)"
  '(lambda (k1)
     (k1 (lambda (f k1)
           (k1 (let ((foo
                      (lambda (foo ls k1)
                        (derefc
                          ls
                          (lambda (v11)
                            (let ((v1 (null? v11)))
                              (if v1
                                  (k1 (empty))
                                  (derefc
                                    f
                                    (lambda (v8)
                                      (derefc
                                        ls
                                        (lambda (v10)
                                          (let ((v9 (car v10)))
                                            (v8 v9
                                                (lambda (v2)
                                                  (derefc
                                                    foo
                                                    (lambda (v4)
                                                      (derefc
                                                        foo
                                                        (lambda (v5)
                                                          (derefc
                                                            ls
                                                            (lambda (v7)
                                                              (let ((v6
                                                                     (cdr v7)))
                                                                (v4 v5
                                                                    v6
                                                                    (lambda (v3)
                                                                      (k1 (cons v2
                                                                                v3)))))))))))))))))))))))))
                 (lambda (ls k1)
                   (derefc
                     foo
                     (lambda (v12)
                       (derefc
                         foo
                         (lambda (v13)
                           (derefc
                             ls
                             (lambda (v14) (v12 v13 v14 k1))))))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-8
  "
 (f let f = 3 in add1(f))"
  '(lambda (k1)
     (derefc
       f
       (lambda (v1)
         (let ((k1 (lambda (v2) (v1 v2 k1))))
           (let ((f 3)) (derefc f (lambda (v3) (k1 (add1 v3))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-9
  "(k (g let g=3 in (f g x)))"
  '(lambda (k1)
     (derefc
       k
       (lambda (v1)
         (derefc
           g
           (lambda (v3)
             (let ((k1 (lambda (v4) (v3 v4 (lambda (v2) (v1 v2 k1))))))
               (let ((g 3))
                 (derefc
                   f
                   (lambda (v5)
                     (derefc
                       g
                       (lambda (v6)
                         (derefc x (lambda (v7) (v5 v6 v7 k1)))))))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-10
  "  (even
  letrec
    even(x) = if zero?(x) then 1 else (odd sub1(x))
    odd(x)  = if zero?(x) then 0 else (even sub1(x))
  in (odd 13))"
  '(lambda (k1)
     (derefc
       even
       (lambda (v1)
         (let ((k1 (lambda (v2) (v1 v2 k1))))
           (letrec ((even
                     (lambda (x k1)
                       (derefc
                         x
                         (lambda (v12)
                           (let ((v8 (zero? v12)))
                             (if v8
                                 (k1 1)
                                 (derefc
                                   odd
                                   (lambda (v9)
                                     (derefc
                                       x
                                       (lambda (v11)
                                         (let ((v10 (sub1 v11)))
                                           (v9 v10 k1))))))))))))
                    (odd
                     (lambda (x k1)
                       (derefc
                         x
                         (lambda (v7)
                           (let ((v3 (zero? v7)))
                             (if v3
                                 (k1 0)
                                 (derefc
                                   even
                                   (lambda (v4)
                                     (derefc
                                       x
                                       (lambda (v6)
                                         (let ((v5 (sub1 v6)))
                                           (v4 v5 k1)))))))))))))
             (derefc odd (lambda (v13) (v13 13 k1)))))))))
