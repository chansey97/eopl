;;; ****************************************************************
;;; ****************************************************************

;;; parser-gen.scm 

;;; Steps in parser generation:

;;; 1.  Eliminate arbno's by making new nonterms with goto's and
;;; emit-list's. 

;;; 2.  Factor productions with common prefixes (not in this version).

;;; 3.  Compute first and follow sets

;;; 4.  Compute prediction table & generate actions

;;; ****************************************************************

;; parser = token-stream * ((tree * token * token-stream) -> ans) -> ans
;; token-stream should be terminated by end-marker token.

(define sllgen:make-parser             
  (lambda (grammar)
    (sllgen:grammar-check-syntax grammar)
    (sllgen:initialize-non-terminal-table! grammar)
    (sllgen:arbno-initialize-table!)
    (let ((parse-table (sllgen:build-parse-table grammar))
          (start-symbol (sllgen:grammar->start-symbol grammar)))
      (lambda (token-stream k)       ; k : (tree * token * token-stream) -> ans
        (sllgen:find-production start-symbol parse-table 
                                '() '() token-stream k)))))


(define the-table 'ignored)

(define sllgen:build-parse-table
  (lambda (grammar)
    (let* ((g (sllgen:eliminate-arbnos-from-productions grammar))
           (first-table (sllgen:first-table g))
           (follow-table (sllgen:follow-table
                          (sllgen:grammar->start-symbol grammar)
                          g
                          first-table))
           (table
            (sllgen:productions->parsing-table g first-table
                                               follow-table)))
      ;       (sllgen:pretty-print first-table)
      ;       (sllgen:pretty-print follow-table)
      ;        (sllgen:pretty-print table)
      (set! the-table table)
      (sllgen:check-table table)
      table)))
