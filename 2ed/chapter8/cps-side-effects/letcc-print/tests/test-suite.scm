(add-test! 'lang8-5print 'two-prints
  "(f print((g x)) print(4))"
  '(lambda (k1)
  (g x
     (lambda (v3)
       (printc
         v3
         (lambda (v1) (printc 4 (lambda (v2) (f v1 v2 k1)))))))))

(add-test! 'lang8-5print 'letcc-no-set-1
  "+(1, letcc j in +(4, throw 3 to j))"
  '(lambda (k1)
     (let ((k1 (lambda (v1) (k1 (+ 1 v1)))))
       (let ((j k1)) (j 3)))))

(add-test! 'lang8-5print 'letcc-no-set-2
  "let x = 4 g = proc(n)*(n,n) in (g letcc g in (f throw x to g))"
  '(lambda (k1)
     (let ((x 4) (g (lambda (n k1) (k1 (* n n)))))
       (let ((k1 (lambda (v1) (g v1 k1)))) (let ((g k1)) (g x))))))
