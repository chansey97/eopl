;;; ****************************************************************

;;; syntax.s :  concrete syntax for grammars, etc.

;;; ****************************************************************

;;; Concrete Syntax for grammars

;;; <Grammar> ::= (<production> ...)  ;; nonterm of first prod is
;;;                                      start symbol.
;;; <production> ::= (lhs rhs action)
;;;
;;; lhs ::= symbol                    ;; a symbol that appears in a
;;;                                      lhs is a non-term, all others
;;;                                      are terminals
;;; rhs ::= (rhs-item ...)
;;;
;;; rhs-item ::= string | symbol | (ARBNO . rhs) | (SEPARATED-LIST nt token)
;;;      
;;; action ::= symbol | EMIT-LIST | (GOTO lhs)
;;; EMIT-LIST and (GOTO lhs) are not allowed in user input.

;;; ****************************************************************

;;; Auxiliaries for dealing with syntax of grammars

;;; need to define sllgen:grammar-check-syntax

(define sllgen:check
  (lambda (test format . args)
    (lambda (obj)
      (or (test obj)
          (apply sllgen:error
                 `(parser-generation ,format . ,args))))))

(define sllgen:grammar-check-syntax
  (lambda (grammar)
    ((sllgen:list-of sllgen:production-check-syntax)
     grammar)))

(define sllgen:production-check-syntax
  (lambda (production)
    ((sllgen:tuple-of
      (sllgen:check symbol? "lhs of production not a symbol: ~s"
                    production)
      (sllgen:rhs-check-syntax production)
      (sllgen:check symbol?
                    "action of production not a symbol: ~s"
                    production))
     production)))

(define sllgen:rhs-check-syntax
  (lambda (production)
    (sllgen:list-of 
     (sllgen:rhs-item-check-syntax production))))

(define sllgen:rhs-item-check-syntax
  (lambda (production)
    (lambda (rhs-item)
      ((sllgen:check
        (sllgen:either
         string?
         symbol?
         (sllgen:pair-of
          (lambda (v) (eqv? v 'arbno))
          (sllgen:rhs-check-syntax production))
         sllgen:really-separated-list?
         )
        "illegal rhs item ~s in production ~s"
        rhs-item production)
       rhs-item))))

(define sllgen:really-separated-list?
  (lambda (rhs-item)
    (and (pair? rhs-item)
         (eq? (car rhs-item) 'separated-list)
         (> (length rhs-item) 2)
         (sllgen:rhs-check-syntax (cdr rhs-item))
         (let
             ; ((last-item (car (last-pair rhs-item))))
             ((last-item (sllgen:last rhs-item)))
           (or (symbol? last-item) (string? last-item))))))

(define sllgen:pair-of
  (lambda (pred1 pred2)
    (lambda (obj)
      (and (pair? obj)
           (pred1 (car obj))
           (pred2 (cdr obj))))))

(define sllgen:list-of
  (lambda (pred)
    (lambda (obj)
      (or (null? obj)
          (and (pair? obj)
               (pred (car obj))
               ((sllgen:list-of pred) (cdr obj)))))))

(define sllgen:tuple-of
  (lambda preds
    (lambda (obj)
      (let loop ((preds preds) (obj obj))
        (if (null? preds)
            (null? obj)
            (and (pair? obj)
                 ((car preds) (car obj))
                 (loop (cdr preds) (cdr obj))))))))

(define sllgen:either
  (lambda preds
    (lambda (obj)
      (let loop ((preds preds))
        (cond
          ((null? preds) #f)
          (((car preds) obj) #t)
          (else (loop (cdr preds))))))))


(define sllgen:grammar->productions 
  (lambda (gram) gram))                 ; nothing else now, but this
; might change

(define sllgen:grammar->start-symbol
  (lambda (gram)
    (sllgen:production->lhs
     (car 
      (sllgen:grammar->productions gram)))))

(define sllgen:make-production
  (lambda (lhs rhs action)
    (list lhs rhs action)))

(define sllgen:production->lhs car)
(define sllgen:production->rhs cadr)
(define sllgen:production->action caddr)

(define sllgen:productions->non-terminals
  (lambda (productions)
    (map sllgen:production->lhs productions)))

(define sllgen:arbno?
  (lambda (rhs-item)
    (and (pair? rhs-item)
         (eq? (car rhs-item) 'arbno))))

(define sllgen:arbno->rhs cdr)

(define sllgen:separated-list?
  (lambda (rhs-item)
    (and (pair? rhs-item)
         (eq? (car rhs-item) 'separated-list)
         (> (length rhs-item) 2))))

;;; (separated-list rhs-item ... separator)

(define sllgen:separated-list->nonterm cadr)

(define sllgen:separated-list->separator 
  (lambda (item)
    (let loop ((items (cdr item)))
      (cond
        ((null? (cdr items)) (car items))
        (else (loop (cdr items)))))))

(define sllgen:separated-list->rhs
  (lambda (item)
    (let loop ((items (cdr item)))
      (cond
        ((null? (cdr items)) '())
        (else (cons (car items) (loop (cdr items))))))))

(define sllgen:goto-action
  (lambda (lhs) (list 'goto lhs)))

(define sllgen:emit-list-action
  (lambda () '(emit-list)))

(define sllgen:grammar->string-literals ; apply this after arbnos have
  ; been eliminated.
  (lambda (grammar)
    (apply append
           (map 
            (lambda (production)
              (sllgen:rhs->string-literals
               (sllgen:production->rhs production)))
            grammar))))

(define sllgen:rhs->string-literals
  (lambda (rhs)
    (let loop ((rhs rhs))
      (cond
        ((null? rhs) '())
        ((string? (car rhs)) (cons (car rhs) (loop (cdr rhs))))
        ((pair? (car rhs)) (append (loop (cdar rhs)) (loop (cdr rhs))))
        (else (loop (cdr rhs)))))))

(define sllgen:grammar->string-literal-scanner-spec
  (lambda (grammar)
    (let ((class (sllgen:gensym 'literal-string)))
      (map
       (lambda (string) (list class (list string) 'make-string))
       (sllgen:grammar->string-literals grammar)))))

;;; ****************************************************************

;;; updatable associative tables

;;; table ::= ((symbol . list) ...)


(define sllgen:make-initial-table       ; makes table with all entries
  ; initialized to empty
  (lambda (symbols)
    (map list symbols)))

(define sllgen:add-value-to-table!
  (lambda (table key value)
    (let ((pair (assq key table)))
      (if (member value (cdr pair))
          #f
          (begin
            (set-cdr! pair (cons value (cdr pair)))
            #t)))))

(define sllgen:table-lookup
  (lambda (table key)
    (cdr (assq key table))))

(define sllgen:uniq
  (lambda (l)
    (if (null? l) '()
        (let ((z (sllgen:uniq (cdr l))))
          (if (member (car l) z)
              z
              (cons (car l) z))))))

(define sllgen:union
  (lambda (s1 s2)                       ; s1 and s2 already unique
    (if (null? s1) s2
        (if (member (car s1) s2)
            (sllgen:union (cdr s1) s2)
            (cons (car s1) (sllgen:union (cdr s1) s2))))))

;; this is only called with '(), so the eqv? is ok.
(define sllgen:rember
  (lambda (a s)
    (cond 
      ((null? s) s)
      ((eqv? a (car s)) (cdr s))
      (else (cons (car s) (sllgen:rember a (cdr s)))))))


(define sllgen:gensym
  (let ((n 0))
    (lambda (s)
      (set! n (+ n 1))
      (let ((s (if (string? s) s (symbol->string s))))
        (string->symbol
         (string-append s (number->string n)))))))


;;; ****************************************************************

;;; a table for keeping the arity of the generated nonterminals for
;;; arbno. 

(define sllgen:arbno-table '())

(define sllgen:arbno-initialize-table!
  (lambda ()
    (set! sllgen:arbno-table '())))

(define sllgen:arbno-add-entry!
  (lambda (sym val)
    (set! sllgen:arbno-table
          (cons (cons sym val) sllgen:arbno-table))))

(define sllgen:arbno-assv
  (lambda (ref)
    (assv ref sllgen:arbno-table)))

(define sllgen:non-terminal-table '())

(define sllgen:initialize-non-terminal-table!
  (lambda (productions)
    (set! sllgen:non-terminal-table '())
    (for-each
     (lambda (prod)
       (sllgen:non-terminal-add!
        (sllgen:production->lhs prod)))
     productions)))

(define sllgen:non-terminal-add!
  (lambda (sym)
    (if (not (memv sym sllgen:non-terminal-table))
        (set! sllgen:non-terminal-table
              (cons sym sllgen:non-terminal-table)))))

(define sllgen:non-terminal?
  (lambda (sym)
    (memv sym sllgen:non-terminal-table)))
