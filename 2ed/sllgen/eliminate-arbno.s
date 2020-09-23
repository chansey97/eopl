;;; ****************************************************************

;;; eliminate-arbno.s

;;; replaces (ARBNO lhs) items with new productions

(define sllgen:eliminate-arbnos-from-rhs
  (lambda (rhs k)
    ;; returns to its continuation the new rhs and the list of
    ;; new productions
    (cond
      ((null? rhs)
       (k rhs '()))
      ((sllgen:arbno? (car rhs))
       (let ((new-nonterm (sllgen:gensym
                           (if (symbol? (cadar rhs)) (cadar rhs) 'arbno)))
             (local-rhs (sllgen:arbno->rhs (car rhs))))
         (sllgen:arbno-add-entry!
          new-nonterm
          (sllgen:rhs-data-length local-rhs))
         (sllgen:eliminate-arbnos-from-rhs
          (cdr rhs)
          (lambda (new-rhs new-prods)
            (sllgen:eliminate-arbnos-from-rhs
             local-rhs
             (lambda (new-local-rhs new-local-prods)
               (k
                (cons new-nonterm new-rhs)  
                (cons
                    (sllgen:make-production
                     new-nonterm '() (sllgen:emit-list-action))
                  (cons
                      (sllgen:make-production
                       new-nonterm
                       new-local-rhs
                       (sllgen:goto-action new-nonterm))
                    (append new-local-prods new-prods))))))))))
      ((sllgen:separated-list? (car rhs))
       ;; A -> ((sep-list B1 B2 ... C) ...)
       (let* ((local-rhs (sllgen:separated-list->rhs (car rhs)))
              (separator (sllgen:separated-list->separator (car rhs)))
              (seed (if (symbol? local-rhs) local-rhs 'seplist))
              (new-nonterm1 (sllgen:gensym seed))
              (new-nonterm2 (sllgen:gensym seed))
              (new-nonterm3 (sllgen:gensym seed)))
         (sllgen:arbno-add-entry!
          new-nonterm1
          (sllgen:rhs-data-length local-rhs))
         (sllgen:eliminate-arbnos-from-rhs
          (cdr rhs)
          (lambda (new-rhs new-prods)
            (sllgen:eliminate-arbnos-from-rhs
             local-rhs
             (lambda (new-local-rhs new-local-prods)
               (k
                (cons new-nonterm1 new-rhs) ; A -> (g1 ...)
                (append
                 (list
                  (sllgen:make-production  ; g1  -> e
                   new-nonterm1 '()
                   (sllgen:emit-list-action)) 
                  (sllgen:make-production  ; g1 -> B1 B2 (goto g3)
                   new-nonterm1 
                   new-local-rhs
                   (sllgen:goto-action new-nonterm3))
                  (sllgen:make-production ; g2 -> B1 B2 (goto g3).
                   new-nonterm2 
                   new-local-rhs
                   (sllgen:goto-action new-nonterm3)) 
                  (sllgen:make-production     ; g3 -> e (emit-list)
                   new-nonterm3
                   '() (sllgen:emit-list-action)) 
                  (sllgen:make-production ; g3 -> C (goto g2)
                   new-nonterm3
                   (list separator)
                   (sllgen:goto-action new-nonterm2)))
                 new-local-prods
                 new-prods))))))))
      (else
       (sllgen:eliminate-arbnos-from-rhs (cdr rhs)
                                         (lambda (new-rhs new-prods)
                                           (k (cons (car rhs) new-rhs)
                                              new-prods)))))))

(define sllgen:eliminate-arbnos-from-production
  (lambda (production)
    ;; returns list of productions
    (sllgen:eliminate-arbnos-from-rhs
     (sllgen:production->rhs production)
     (lambda (new-rhs new-prods)
       (let ((new-production
              (sllgen:make-production 
               (sllgen:production->lhs production)
               new-rhs
               (sllgen:production->action production))))
         (cons new-production
           (sllgen:eliminate-arbnos-from-productions new-prods)))))))

(define sllgen:eliminate-arbnos-from-productions
  (lambda (productions)
    (let loop ((productions productions))
      (if (null? productions)
          '()
          (append
           (sllgen:eliminate-arbnos-from-production (car productions))
           (loop (cdr productions)))))))

(define sllgen:rhs-data-length
  (lambda (rhs)
    (let ((report-error
           (lambda (rhs-item msg)
             (sllgen:error 'parser-generation
                           "illegal item ~s (~a) in rhs ~s"
                           rhs-item msg rhs))))
      (letrec
          ((loop
            (lambda (rhs)
              ;; (eopl:printf "~s~%" rhs)
              (if (null? rhs) 0
                  (let ((rhs-item (car rhs))
                        (rest (cdr rhs)))
                    (cond
                      ((and
                        (symbol? rhs-item)
                        (sllgen:non-terminal? rhs-item))
                       ;                    (eopl:printf "found nonterminal~%")
                       (+ 1 (loop rest)))
                      ((symbol? rhs-item)
                       ;                    (eopl:printf "found terminal~%")
                       (+ 1 (loop rest)))
                      ((sllgen:arbno? rhs-item)
                       ;                    (eopl:printf "found arbno~%")
                       (+
                        (loop (sllgen:arbno->rhs rhs-item))
                        (loop rest)))
                      ((sllgen:separated-list? rhs-item)
                       ;                     (eopl:printf "found seplist~%")
                       (+
                        (loop (sllgen:separated-list->rhs rhs-item))
                        (loop rest)))
                      ((string? rhs-item)
                       ;                     (eopl:printf "found string~%")
                       (loop rest))
                      (else
                       ;                      (eopl:printf "found error~%")
                       (report-error rhs-item "unrecognized item"))))))))
        (loop rhs)))))
