;;; ****************************************************************

;;; first-and-follow.s

;;; calculate first and follow sets

;;; base conditions:

;;; A -> a ...   => a in first(A)
;;; A -> ()      => nil in first(A)

;;; closure conditions:

;;; A -> (B1 ... Bk c ...) & nil in first(B1)...first(Bk) => c in first(A)
;;; A -> (B1 ... Bk C ...) & nil in first(B1)...first(Bk) & c in first(C) =>
;;;                                                         c in first(A) 
;;; A -> (B1 ... Bk) & nil in first(B1)...first(Bk) => nil in first(A)

(define sllgen:first-table
  (lambda (productions)
    (let* ((non-terminals
            (sllgen:uniq (map sllgen:production->lhs productions)))
           (table (sllgen:make-initial-table non-terminals)))
      (letrec
          ((loop 
            ;; initialize with the base conditions and return the
            ;; productions to be considered for the closure
            (lambda (productions)
              (cond
                ((null? productions) '())
                ((null? (sllgen:production->rhs (car productions)))
                 ;; A -> ()      => nil in first(A)
                 (sllgen:add-value-to-table! table 
                                             (sllgen:production->lhs (car productions))
                                             '())
                 (loop (cdr productions)))
                ((member (car
                          (sllgen:production->rhs
                           (car productions)))
                         non-terminals)
                 ;; this one is for the closure
                 (cons (car productions)
                   (loop (cdr productions))))
                (else
                 ;; this one must start with a terminal symbol
                 (sllgen:add-value-to-table! table
                                             (sllgen:production->lhs (car productions))
                                             (car
                                              (sllgen:production->rhs
                                               (car productions)))))))))
        (let ((closure-productions (loop productions)))
          (sllgen:iterate-over-first-table table productions
                                           non-terminals)))))) 


(define sllgen:iterate-over-first-table
  (lambda (table productions non-terminals)
    (let* ((changed? '**uninitialized**)
           (add-value!
            (lambda (key value)
              (let ((not-there?
                     (sllgen:add-value-to-table! table key value)))
                (set! changed? (or changed? not-there?)))))
           (first (lambda (key) (sllgen:table-lookup table key))))
      (letrec
          ((rhs-loop
            (lambda (lhs rhs)
              ;; assume everything in the rhs up to this point has () in
              ;; its first set
              (cond
                ((null? rhs)
                 ;; A -> (B1 ... Bk) & nil in first(B1)...first(Bk) =>
                 ;; nil in first(A) 
                 (add-value! lhs '()))
                ;; A -> (B1 ... Bk C ...) & nil in first(B1)...first(Bk)
                ((member (car rhs) non-terminals)
                 (for-each 
                  (lambda (sym)
                    (if (not (null? sym))
                        ;; & c in first(C) => c in first(A) 
                        (add-value! lhs sym)
                        ;; e in first(C) -- continue to search down rhs
                        (rhs-loop lhs (cdr rhs))))
                  (first (car rhs))))
                (else
                 ;; A -> (B1 ... Bk c ...) & nil in
                 ;; first(B1)...first(Bk) => c in first(A) 
                 (add-value! lhs (car rhs))))))
           (main-loop
            (lambda ()
              (set! changed? #f)
              (for-each
               (lambda (production)
                 (rhs-loop 
                  (sllgen:production->lhs production)
                  (sllgen:production->rhs production)))
               productions)
              (if changed?
                  (main-loop)
                  table))))
        (main-loop)))))

(define sllgen:first-of-list
  (lambda (first-table non-terminals items)
    (let ((get-nonterminal
           (lambda (item) 
             (cond
               ((member item non-terminals) item)
               ((symbol? item) #f)
               ((string? item) #f)
               ((eq? (car item) 'goto) (cadr item))
               (else #f)))))
      (letrec
          ((loop (lambda (items)
                   (cond
                     ((null? items) '(()))                 ; ans = {e}
                     ((get-nonterminal (car items)) =>
                                                    (lambda (nonterminal)
                                                      (let ((these
                                                             (sllgen:table-lookup first-table nonterminal)))
                                                        (if (member '() these)
                                                            (let ((others (loop (cdr items))))
                                                              (let inner ((these these))
                                                                (cond
                                                                  ((null? these) others)
                                                                  ((null? (car these))
                                                                   (inner (cdr these)))
                                                                  ((member (car these) others)
                                                                   (inner (cdr these)))
                                                                  (else
                                                                   (cons (car these)
                                                                     (inner (cdr these)))))))
                                                            these))))
                     (else (list (car items)))))))
        (loop items)))))

(define sllgen:follow-table
  (lambda (start-symbol productions first-table)
    (let* ((non-terminals
            (sllgen:uniq (map sllgen:production->lhs productions)))
           (table (sllgen:make-initial-table non-terminals))
           (changed? '**uninitialized**)
           (sllgen:add-value!
            (lambda (key value)
              (let ((not-there?
                     (sllgen:add-value-to-table! table key value)))
                (set! changed? (or changed? not-there?)))))
           ;; closure-rules ::= ((a b) ...) means follow(a) \subset
           ;; follow(b)
           (closure-rules '())
           (get-nonterminal
            (lambda (item) 
              (cond
                ((member item non-terminals) item)
                (else #f)))))
      (sllgen:add-value! start-symbol 'end-marker)
      (letrec
          ((init-loop
            ;; loops through productions once, adding starting values
            ;; to follow-table and other productions to closure-rules
            (lambda (productions)
              (if (null? productions)
                  #t
                  (let* ((production (car productions))
                         (lhs (sllgen:production->lhs production))
                         (rhs (sllgen:production->rhs production))
                         (action (sllgen:production->action production)))
                    (rhs-loop
                     lhs
                     (append rhs ;; add back the goto as a nonterminal 
                             (if (and (pair? action) (eq? (car action) 'goto))
                                 (list (cadr action))
                                 '())))
                    (init-loop (cdr productions))))))
           (rhs-loop
            (lambda (lhs rhs)
              ;; (eopl:printf "rhs-loop lhs=~s rhs=~s~%" lhs rhs)
              (cond
                ((null? rhs) #t)
                ((get-nonterminal (car rhs)) =>
                                             (lambda (nonterminal)
                                               ;; we've found a nonterminal.  What's it followed by?
                                               (let* ((rest (cdr rhs))
                                                      (first-of-rest
                                                       (sllgen:first-of-list
                                                        first-table non-terminals rest)))
                                                 (for-each 
                                                  (lambda (sym)
                                                    (if (not (null? sym))
                                                        ;; A -> (... B C ...) => first(C...) \subset follow(B)
                                                        (sllgen:add-value! nonterminal sym)
                                                        ;; A -> (... B C ...) & e \in first(C ...) =>
                                                        ;; follow(A) \subset follow (B)
                                                        (begin
                                                          (set! closure-rules
                                                                (cons (list lhs nonterminal)
                                                                  closure-rules))
                                                          ;; (eopl:printf "~s~%" (list lhs nonterminal))
                                                          )))
                                                  first-of-rest))
                                               ;; now keep looking
                                               (rhs-loop lhs (cdr rhs))))
                (else
                 ;; this one's not a non-terminal.  Keep looking.
                 (rhs-loop lhs (cdr rhs))))))
           (closure-loop
            (lambda ()
              (set! changed? #f)
              (for-each
               (lambda (rule)
                 (let ((a (car rule))
                       (b (cadr rule)))
                   ;; follow(a) \subset follow(b)
                   (for-each
                    (lambda (sym)
                      (sllgen:add-value! b sym))
                    (sllgen:table-lookup table a))))
               closure-rules)
              (if changed?
                  (closure-loop)
                  table))))
        (init-loop productions)
        ;       (sllgen:pretty-print closure-rules)
        (closure-loop)))))
