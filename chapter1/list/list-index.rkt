; EOPL exercise 1.23
;
; (list-index pred lst) returns the 0-based position of the first element of
; lst that satisfies the predicate pred. If no element of lst satisfies the
; predicate, then list-index returns #f.
;
; > (list-index number? '(a 2 (1 3) b 7))
; 1
; > (list-index symbol? '(a (b c) 17 foo))
; 0
; > (list-index symbol? '(1 2 (a b) 3))
; #f

#lang racket

(define (list-index pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
          0
          (let ((result (list-index pred (cdr lst))))
            (if result
                (+ 1 result)
                #f)))))

(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))

(define (list-index-iter pred lst)
  (define (iter lst counter)
    (if (null? lst)
        #f
        (if (pred (car lst))
            counter
            (iter (cdr lst) (+ counter 1)))))
  (iter lst 0))

(list-index-iter number? '(a 2 (1 3) b 7))
(list-index-iter symbol? '(a (b c) 17 foo))
(list-index-iter symbol? '(1 2 (a b) 3))
