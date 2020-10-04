;;; ****************************************************************

;;; gen-table.s

;;; gen-table.s  take list of productions, first and follow tables,
;;; and generate parsing table

;;; table ::= ((non-terminal (list-of-items action ...)....) ...)

;;; the list of items is the first(rhs) for each production (or
;;; follow(lhs) if the production is empty.  We should probably check
;;; to see that these are non-intersecting, but we probably won't on
;;; this pass.

;;; First thing to do: collect all the productions for a given
;;; non-terminal.  This gives data structure of the form

;;; ((lhs production ...) ...)

;;; We'll do this using updatable tables.

(define sllgen:group-productions
  (lambda (productions)
    (let* ((non-terminals
            (sllgen:uniq (map sllgen:production->lhs productions)))
           (table (sllgen:make-initial-table non-terminals)))
      (for-each
       (lambda (production)
         (let 
             ((lhs (sllgen:production->lhs production)))
           (sllgen:add-value-to-table! table lhs production)))
       productions)
      table)))

;; this one uses the list structure of tables.  [Watch out]

(define sllgen:productions->parsing-table
  (lambda (productions first-table follow-table)
    (let ((non-terminals
           (sllgen:uniq (map sllgen:production->lhs productions)))
          (table (sllgen:group-productions productions)))
      (map 
       (lambda (table-entry)
         (sllgen:make-parse-table-non-terminal-entry
          (car table-entry)
          (map
           (lambda (production)
             (sllgen:make-parse-table-production-entry
              production non-terminals first-table follow-table))
           (cdr table-entry))))
       table))))

(define sllgen:make-parse-table-non-terminal-entry
  (lambda (lhs entries)
    (cons lhs entries)))

(define sllgen:make-parse-table-production-entry
  (lambda (production non-terminals first-table follow-table)
    (let* ((rhs (sllgen:production->rhs production))
           (first-of-rhs (sllgen:first-of-list
                          first-table non-terminals
                          (sllgen:production->rhs production)))
           (steering-items
            (if (member '() first-of-rhs)
                (sllgen:union
                 (sllgen:table-lookup
                  follow-table
                  (sllgen:production->lhs production))
                 (sllgen:rember '() first-of-rhs))
                first-of-rhs)))
      (cons steering-items
        (sllgen:make-parse-table-rhs-entry
         non-terminals
         (sllgen:production->rhs production)
         (sllgen:production->action production))))))

(define sllgen:make-parse-table-rhs-entry
  (lambda (non-terminals rhs action)
    (let loop ((rhs rhs))
      (cond
        ((null? rhs) 
         ;; at end -- emit reduce action or emit-list action
         (if (symbol? action)
             ;; symbols become "reduce",
             ;; (emit-list) and (goto nt) stay the same
             (list (list 'reduce action))
             (list action)))
        ((sllgen:arbno-assv (car rhs)) =>
                                       (lambda (pair)                 ; (cdr pair) is the count for
                                         ; the arbno 
                                         (cons
                                             (list 'arbno (car rhs) (cdr pair))
                                           (loop (cdr rhs)))))
        ((member (car rhs) non-terminals)
         (cons (list 'non-term (car rhs))
           (loop (cdr rhs))))
        ((symbol? (car rhs))
         (cons (list 'term (car rhs))
           (loop (cdr rhs))))
        ((string? (car rhs))
         (cons (list 'string (car rhs))
           (loop (cdr rhs))))
        (else
         (sllgen:error 'parser-generation
                       "unknown rhs entry ~s"
                       (car rhs)))))))
