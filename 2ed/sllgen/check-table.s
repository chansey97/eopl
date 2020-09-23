;;; ****************************************************************

;;; check-table.s

;;; take a parse table and check for conflicts

;;; table ::= ((non-terminal (list-of-items action ...)....) ...)

(define sllgen:check-table
  (lambda (table)
    (for-each sllgen:check-productions table)))

(define sllgen:check-productions
  (lambda (non-terminal-entry)
    (let ((non-terminal (car non-terminal-entry))
          (productions (cdr non-terminal-entry)))
      ;; see if the list-of-items are pairwise disjoint
      (let loop ((productions productions))
        (if (null? productions)
            #t                            ; no more to check
            (let ((this-production (car productions))
                  (other-productions (cdr productions)))
              ;; check this production
              (for-each
               (lambda (class)
                 (let inner ((others other-productions))
                   (cond
                     ((null? others) #t)
                     ;; memq changed to member Tue Nov 16 14:26:32
                     ;; 1999, since class could be a string.
                     ((member class (car (car others)))
                      (sllgen:error 'parser-generation
                                    "~%Grammar not LL(1): Shift conflict detected for class ~s in nonterminal ~s:~%~s~%~s~%"
                                    class non-terminal this-production (car others)))
                     (else (inner (cdr others))))))
               (car this-production))
              ;; and check the others
              (loop other-productions)))))))
