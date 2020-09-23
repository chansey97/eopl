;;; ****************************************************************        

;;; parse.s

;;; parse.s -- run the generated parser

;;; parsing table is of following form:

;;; table ::= ((non-terminal alternative ...) ...)
;;; alternative ::= (list-of-items action ...) 
;;; action ::= (TERM symbol) | (NON-TERM symbol) | (GOTO symbol) 
;;;            | (EMIT-LIST) | (REDUCE symbol)

;;; The token register can either contain an token or '() -- the latter
;;; signifying an empty buffer, to be filled when necessary.

; (define-record sllgen:parser-result (tree token stream))

; k = (lambda (tree token stream) ...)
; token may be a token or nil.
(define sllgen:find-production
  (lambda (non-terminal parser buf token stream k)
    (if (null? token)
        (sllgen:stream-get! stream
                            (lambda (next-token next-stream)
                              ;        '(eopl:printf "find-production: filling token buffer with ~s~%" token)
                              (set! token next-token)
                              (set! stream next-stream))
                            (lambda ()
                              (sllgen:error 'sllgen:find-production
                                            "~%Internal error: shouldn't run off end of stream"))))
    ;    '(eopl:printf "sllgen:find-production: nonterminal = ~s token = ~s~%"
    ;       non-terminal token)
    (let loop
        ((alternatives (cdr (assq non-terminal parser))))
      (cond
        ((null? alternatives)
         (sllgen:error 'parsing
                       "at line ~s~%Nonterminal <~s> can't begin with ~s ~s"
                       (sllgen:token->location token)
                       non-terminal
                       (sllgen:token->class token)
                       (sllgen:token->data token)))
        ((member (sllgen:token->class token) (car (car alternatives)))
         ;         '(eopl:printf "sllgen:find-production: using ~s~%~%"
         ;            (cdr (car alternatives)))
         (sllgen:apply-actions non-terminal (cdr (car alternatives))
                               parser buf token stream k))
        ((and (string? (sllgen:token->data token))
              (member (sllgen:token->data token) (car (car alternatives))))
         (sllgen:apply-actions non-terminal (cdr (car alternatives))
                               parser buf token stream k))
        (else (loop (cdr alternatives)))))))

(define sllgen:apply-actions
  (lambda (lhs action-list parser buf token stream k)
    (let loop ((actions action-list)
               (buf buf)
               (token token)
               (stream stream))
      (let ((fill-token!                ; fill-token! is a macro in mzscheme
             (lambda ()
               (if (null? token)
                   (sllgen:stream-get! stream
                                       (lambda (next-token next-stream)
                                         (set! token next-token)
                                         (set! stream next-stream))
                                       (lambda ()
                                         (sllgen:error 'sllgen:apply-actions
                                                       "~%Internal error: shouldn't run off end of stream"
                                                       ))))))
            (report-error
             (lambda (target)
               (sllgen:error 'parsing
                             "at line ~s~%Looking for ~s, found ~s ~s in production~%~s"
                             (sllgen:token->location token)
                             target
                             (sllgen:token->class token)
                             (sllgen:token->data token)
                             action-list))))
        (let ((action      (car actions))
              (next-action (cdr actions)))
          ;         (eopl:printf "actions = ~s~%token = ~s buf = ~s~%~%" actions token buf)
          (case (car action)
            ((term)
             (fill-token!)
             (let ((class (cadr action)))
               (if (eq? (sllgen:token->class token) class)
                   ;; ok, this matches, proceed, but don't get next token --
                   ;; after all, this might be the last one.
                   (loop next-action 
                         (cons (sllgen:token->data token) buf)
                         '()                  ; token register is now empty
                         stream)
                   ;; nope, fail.
                   (report-error class))))
            ((string)
             (let ((the-string (cadr action)))
               (fill-token!)
               (if (and
                    (not (eq? (sllgen:token->class token) 'end-marker))
                    (string? (sllgen:token->data token))
                    (string=? (sllgen:token->data token) the-string))
                   (loop next-action buf '() stream)
                   ;; nope, fail.
                   (report-error the-string))))
            ((non-term)
             (let ((non-terminal (cadr action)))
               (sllgen:find-production non-terminal parser
                                       '() token stream
                                       (lambda (tree token stream)
                                         (loop next-action (cons tree buf) token stream)))))
            ((arbno)
             (let ((non-terminal (cadr action))
                   (count       (caddr action)))
               (sllgen:find-production non-terminal parser
                                       '() token stream
                                       (lambda (trees token stream)
                                         (loop next-action
                                               (sllgen:unzip-buffer trees count buf)
                                               token stream)))))

            ((goto)
             (let ((non-terminal (cadr action)))
               (sllgen:find-production non-terminal parser buf token
                                       stream k)))
            ((emit-list)
             (k buf token stream))
            ((reduce)
             (let ((opcode (cadr action)))
               (k
                ;               (apply (make-record-from-name opcode)
                ;                      (reverse buf))
                (sllgen:apply-reduction lhs opcode (reverse buf))
                token
                stream)))
            (else
             (sllgen:error 'sllgen:apply-actions
                           "~%Internal error: unknown instruction ~s"
                           action))))))))

(define sllgen:unzip-buffer
  (lambda (trees n buf)
    (let ((ans (let consloop ((n n))
                 (if (zero? n) buf (cons '() (consloop (- n 1)))))))
      (let loop ((trees trees)
                 (ptr ans)
                 (ctr n))
        ;     (eopl:printf "ctr = ~s trees = ~s~%" ctr trees)
        (cond
          ((null? trees) ans)
          ((zero? ctr) (loop trees ans n))
          (else
           (set-car! ptr (cons (car trees) (car ptr)))
           (loop (cdr trees) (cdr ptr) (- ctr 1))))))))

;;; next are several alternative definitions for sllgen:apply-reduction

;; just a stub.
(define sllgen:apply-reduction-stub
  (lambda (lhs prod-name args)
    (sllgen:error 'sllgen-configuration "need to set sllgen:apply-reduction")))

;; just construct a list
(define sllgen:generic-node-constructor
  (lambda (lhs production-name args)
    (cons production-name args)))

;; this is the default, since it lets sllgen stand alone.
(define sllgen:apply-reduction sllgen:generic-node-constructor)

;; here are alternative bindings:

;; Alas, define-datatype:constructor-back-door no longer exists.  This
;; was supposed to be a way to get at the constructor from its name,
;; so that we could write sllgen without knowing the representation of
;; trees used by define-datatype.  But (alas!) it no longer exists.
;; Maybe it will be put back in some future version of d-d.
;; --mw Mon Apr 24 15:07:20 2000 
;
;; look up the constructor in the define-datatype table
; (define sllgen:apply-datatype-backdoor
;   (lambda (lhs variant-name args)
;     (apply (define-datatype:constructor-back-door lhs variant-name)
;       args)))

(define sllgen:eval-reduction
  (lambda (lhs opcode args)
    (apply (eval opcode (interaction-environment))
           args)))

;; and procedures to install them:

(define sllgen:use-generic-node-constructor
  (lambda ()
    (set! sllgen:apply-reduction sllgen:generic-node-constructor)))

; (define sllgen:use-datatype-backdoor
;   (lambda ()
;     (set! sllgen:apply-reduction sllgen:apply-datatype-backdoor)))

(define sllgen:use-eval-reduction
  (lambda ()
    (set! sllgen:apply-reduction sllgen:eval-reduction)))


;; temporary
; (define make-record-from-name
;   (lambda (sym)
;     (lambda args
;       (cons sym args))))


;;; ****************************************************************        

;; go through a grammar and generate the appropriate define-datatypes.

;;; define-datatype syntax is:
;;;(define-datatype Type-name Predicate-name
;;;  (Variant-name (Field-name Predicate-exp) ...) ...)


(define sllgen:build-define-datatype-definitions
  (lambda (scanner-spec grammar)
    (let* ((scanner-datatypes-alist
            (sllgen:make-scanner-datatypes-alist scanner-spec))
           (non-terminals
            (sllgen:uniq (map sllgen:production->lhs
                              (sllgen:grammar->productions grammar))))
           (datatype-table (sllgen:make-initial-table non-terminals)))
      ;; for each production, add an entry to the table.  Each entry is
      ;; (prod-name . datatype-list)
      (for-each
       (lambda (production)
         (sllgen:add-value-to-table! datatype-table
                                     (sllgen:production->lhs production)
                                     (cons
                                         (sllgen:production->action production)
                                       (sllgen:make-rhs-datatype-list 
                                        (sllgen:production->rhs production)
                                        non-terminals
                                        scanner-datatypes-alist))))
       (sllgen:grammar->productions grammar))
      ;; now generate the list of datatypes for each table entry
      (map
       (lambda (non-terminal)
         (sllgen:make-datatype-definition non-terminal
                                          (sllgen:table-lookup datatype-table non-terminal)))
       non-terminals))))


(define sllgen:make-scanner-datatypes-alist
  (lambda (init-states)
    (let
        ((opcode-type-alist
          '((make-symbol . symbol?)
            (symbol . symbol?)
            (make-string . string?)
            (string . string?)
            (make-number . number?)
            (number . number?))))
      (let loop ((init-states init-states))
        (if (null? init-states) '()
            (let ((init-state  (car init-states))
                  (init-states (cdr init-states)))
              (let ((class (car init-state))
                    (type-pair (assq (sllgen:last init-state) opcode-type-alist)))
                (if (not type-pair)
                    (loop init-states)
                    (cons (cons class (cdr type-pair)) 
                      (loop init-states))))))))))

(define sllgen:last
  (lambda (x)
    (and
     (or (pair? x)
         (sllgen:error 'sllgen:last "~%Can't take last of non-pair ~s" x))
     (if (null? (cdr x))
         (car x)
         (sllgen:last (cdr x))))))

;;; rhs ::= (rhs-item ...)
;;;
;;; rhs-item ::= string | symbol | (ARBNO . rhs) | (SEPARATED-LIST rhs
;;;                                                         token)  

(define sllgen:make-rhs-datatype-list
  (lambda (rhs non-terminals scanner-datatypes-alist)
    (let ((report-error
           (lambda (rhs-item msg)
             (sllgen:error 'defining-datatypes
                           "~%Illegal item ~s (~a) in rhs ~s"
                           rhs-item msg rhs))))
      (let loop ((rhs rhs))
        (if (null? rhs) '()
            (let ((rhs-item (car rhs))
                  (rest (cdr rhs)))
              (cond
                ((and (symbol? rhs-item) (member rhs-item non-terminals))
                 ;; this is a non-terminal
                 (cons (sllgen:non-terminal->tester-name rhs-item)
                   (loop rest)))
                ((symbol? rhs-item)
                 ;; this must be a terminal symbol
                 (let ((type (assq rhs-item scanner-datatypes-alist)))
                   (if type
                       (cons (cdr type) (loop rest))
                       (report-error rhs-item "unknown symbol"))))
                ((sllgen:arbno? rhs-item)
                 (append
                  (map 
                   (lambda (x) (list 'list-of x))
                   (loop (sllgen:arbno->rhs rhs-item)))
                  (loop rest)))
                ((sllgen:separated-list? rhs-item)
                 (append
                  (map 
                   (lambda (x) (list 'list-of x))
                   (loop (sllgen:separated-list->rhs rhs-item)))
                  (loop rest)))
                ((string? rhs-item)
                 (loop rest))
                (else (report-error rhs-item "unrecognized item")))))))))

(define sllgen:non-terminal->tester-name
  (lambda (x)
    (string->symbol (string-append (symbol->string x) "?"))))

;; variants are now the same as constructors
(define sllgen:variant->constructor-name
  (lambda (x) x))


(define sllgen:make-datatype-definition
  (lambda (non-terminal entries)
    (let ((tester-name
           (sllgen:non-terminal->tester-name non-terminal))
          (entries 
           ;; reverse gets the entries in the same order as the productions
           (map sllgen:make-variant (reverse entries))))
      `(define-datatype ,non-terminal ,tester-name . ,entries))))

(define sllgen:make-variant
  (lambda (entry)
    `(,(car entry)
      . ,(map
          (lambda (pred)
            (list (sllgen:gensym (car entry)) pred))
          (cdr entry)))))
