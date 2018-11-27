#lang racket
(require rackunit rackunit/text-ui)
(require eopl)
(require "../bigits.rkt")

; Factorial with bignums

(define (factorial bignum)
  (if (is-zero? bignum)
      (successor (zero))
      (multiply bignum (factorial (predecessor bignum)))))

; Benchmarking

(define (benchmark-in-base base-to-use)
  (parameterize ((base base-to-use))
    (printf "Running factorial in base ~s:\n" (base))
    (for ([n (in-range 6 11)])
         (benchmark-factorial n))))

(define (benchmark-factorial n)
  (printf "  ~s! (base ~s): " n (base))
  (time (factorial (int->bignum n))))

(define (run-benchmarks)
  (benchmark-in-base 2)
  (benchmark-in-base 4)
  (benchmark-in-base 10)
  (benchmark-in-base 1000)
  (benchmark-in-base 100000))

; Finally, the results:
;
; Running factorial in base 2:
;   6! (base 2): cpu time: 0 real time: 1 gc time: 0
;   7! (base 2): cpu time: 4 real time: 3 gc time: 0
;   8! (base 2): cpu time: 30 real time: 30 gc time: 0
;   9! (base 2): cpu time: 267 real time: 268 gc time: 11
;   10! (base 2): cpu time: 2530 real time: 2532 gc time: 24
; Running factorial in base 4:
;   6! (base 4): cpu time: 0 real time: 0 gc time: 0
;   7! (base 4): cpu time: 3 real time: 3 gc time: 0
;   8! (base 4): cpu time: 19 real time: 18 gc time: 0
;   9! (base 4): cpu time: 161 real time: 161 gc time: 3
;   10! (base 4): cpu time: 1561 real time: 1563 gc time: 14
; Running factorial in base 10:
;   6! (base 10): cpu time: 0 real time: 1 gc time: 0
;   7! (base 10): cpu time: 2 real time: 1 gc time: 0
;   8! (base 10): cpu time: 14 real time: 15 gc time: 1
;   9! (base 10): cpu time: 133 real time: 133 gc time: 0
;   10! (base 10): cpu time: 1268 real time: 1269 gc time: 13
; Running factorial in base 1000:
;   6! (base 1000): cpu time: 0 real time: 1 gc time: 0
;   7! (base 1000): cpu time: 2 real time: 1 gc time: 0
;   8! (base 1000): cpu time: 12 real time: 13 gc time: 0
;   9! (base 1000): cpu time: 114 real time: 113 gc time: 2
;   10! (base 1000): cpu time: 1114 real time: 1115 gc time: 15
; Running factorial in base 100000:
;   6! (base 100000): cpu time: 0 real time: 0 gc time: 0
;   7! (base 100000): cpu time: 3 real time: 2 gc time: 0
;   8! (base 100000): cpu time: 13 real time: 14 gc time: 0
;   9! (base 100000): cpu time: 113 real time: 113 gc time: 2
;   10! (base 100000): cpu time: 1116 real time: 1119 gc time: 14
;
; There is just too much recursion happening in order for me to be willing to
; analyze why those are the results. successor and predecessor don't work in
; constant time, which makes me unwilling to analyze how multiple and add
; interact with them.
;
; If you want to reproduce the results, just uncomment the following line:
;
;; (run-benchmarks)

;; Conclusions:
;; 1.  The larger the argument is, the longer the execution time is.
;; This is because of inherent time complexity of factorial.
;; 2.  The larger the base is, the shorter the execution time is.
;; Because larger base takes fewer bigits to represent a given number.
