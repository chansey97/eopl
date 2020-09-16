;;; This file contains all of the Scheme programs and Scheme program
;;; fragments in the book.  This will make it possible to avoid
;;; typing in all the code.
;;; 
;;; The code is largely compliant with the Revised^3.99 Report,
;;; except for: 
;;; 
;;;   (1) the use of the macros variant-case and define-record.
;;; These are defined in Appendix A (see appendixa.ss), but you may
;;; need to rewrite these definitions to fit your local Scheme
;;; implementation.
;;; 
;;;   (2) the use of the procedure (error ....) .  You may need to
;;; redefine this procedure to fit your local Scheme implementation.
;;; 
;;; The entire file loads if appendixa.ss is loaded first.
;;; Because many of the code segments are fragments of larger
;;; programs, the fact that the file loads does not imply that the
;;; programs will work.  Whenever ...  appeared within a cond, case,
;;; or variant-case representing the remaining clauses, it has been
;;; replaced by (else ...).
;;; 
;;; It is possible to access a particular Scheme program by knowing
;;; its name, but be wary of the fact that many programs have the
;;; same name.  A better plan is to search for code by figure number
;;; or page number.

;;; Programs with %%%%%% are for a new printing.

;;; ****************************************************************

;;; Beginning of figures for Chapter 1

;;; Anonymous figure : page 25

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;;; Exercise 1.3.2 : page 26 

(define cell-tag "cell")

(define make-cell
  (lambda (x)
    (vector cell-tag x)))

(define cell?
  (lambda (x)
    (if (vector? x) 
        (if (= (vector-length x) 2)
            (eq? (vector-ref x 0) cell-tag)
            #f)
        #f)))

(define cell-ref
  (lambda (x)
    (if (cell? x)
        (vector-ref x 1)
        (error "Invalid argument to cell-ref:" x))))

;;; End of figures for Chapter 1

;;; ****************************************************************

;;; Beginning of figures for Chapter 2

;;; Anonymous figure : page 39

(define e
  (lambda (n x)
    (if (zero? n)
        1
        (* x
           (e (- n 1) x)))))

;;; Anonymous figure : page 42

(define list-of-numbers?
  (lambda (lst)
    (if (null? lst)
        #t
        (if (pair? lst)
            (if (number? (car lst))
                (list-of-numbers? (cdr lst))
                #f)
            #f))))

;;; Anonymous figure : page 43

(define nth-elt
  (lambda (lst n)
    (if (null? lst)
        (error "nth-elt: list too short")
        (if (zero? n)
            (car lst)
            (nth-elt (cdr lst) (- n 1))))))

;;; Anonymous figure : page 44

(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

;;; Anonymous figure : page 46

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

;;; Anonymous figure : page 47

(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? (car los) s)
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

;;; Anonymous figure : page 48, 49

(define subst
  (lambda (new old slst)
    (if (null? slst)
        '()
        (if (symbol? (car slst))
            (if (eq? (car slst) old)
                (cons new (subst new old (cdr slst)))
                (cons (car slst) (subst new old (cdr slst))))
            (cons (subst new old (car slst))
                  (subst new old (cdr slst)))))))

(define subst
  (lambda (new old slst)
    (if (null? slst)
        '()
        (cons (subst-symbol-expression new old (car slst))
              (subst new old (cdr slst))))))

(define subst-symbol-expression
  (lambda (new old se)
    (if (symbol? se)
        (if (eq? se old) new se)
        (subst new old se))))

(define list-sum
  (lambda (lon)
    (if (null? lon)
        0
        (+ (car lon) 
           (list-sum (cdr lon))))))

;;; Anonymous figure : page 50

(define partial-vector-sum
  (lambda (von n)
    (if (zero? n)
        0
        (+ (vector-ref von (- n 1))
           (partial-vector-sum von (- n 1))))))

(define vector-sum
  (lambda (von)
    (partial-vector-sum von (vector-length von))))

;;; End of figures for Chapter 2

;;; ****************************************************************

;;; Beginning of figures for Chapter 3

;;; Anonymous figure : page 69

(define subst
  (lambda (new old slst)
    (if (null? slst)
        '()
        (let ((car-value (car slst))
              (cdr-result (subst new old (cdr slst))))
          (if (symbol? car-value)
              (if (eq? car-value old)
                  (cons new cdr-result)
                  (cons car-value cdr-result))
              (cons (subst new old car-value)
                    cdr-result))))))

;;; Figure 3.1.1 : page 73

(define vector-sum
  (lambda (von)
    (letrec ((partial-vector-sum
               (lambda (n)
                 (if (zero? n)
                     0
                     (+ (vector-ref von (- n 1))
                        (partial-vector-sum (- n 1)))))))
      (partial-vector-sum (vector-length von)))))

;;; Figure 3.1.2 : page 73

(letrec ((even? (lambda (n) 
                  (if (zero? n) 
                      #t 
                      (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (zero? n)
                     #f 
                     (even? (- n 1))))))
  (even? 3))

;;; Anonymous figure : page 77

(define subst
  (lambda (new old slst)
    (cond
      ((null? slst) '())
      ((symbol? (car slst))
       (if (eq? (car slst) old)
           (cons new (subst new old (cdr slst)))
           (cons (car slst) (subst new old (cdr slst)))))
      (else (cons (subst new old (car slst))
                  (subst new old (cdr slst)))))))

;;; Anonymous figure : page 80, 81, 82

(define leaf-sum
  (lambda (tree)
    (cond 
      ((number? tree) tree)
      ((interior? tree) (+ (leaf-sum (interior->left-tree tree))
                           (leaf-sum (interior->right-tree tree))))
      (else (error "leaf-sum: Invalid tree" tree)))))

(define-record leaf (number))

(define leaf-sum
  (lambda (tree)
    (variant-case tree
      (leaf (number) number)
      (interior (left-tree right-tree)
        (+ (leaf-sum left-tree) (leaf-sum right-tree)))
      (else (error "leaf-sum: Invalid tree" tree)))))

(define leaf-sum
  (lambda (tree)
    (let ((*record* tree))
      (cond 
        ((leaf? *record*)
         (let ((number (leaf->number *record*)))
           number))
        ((interior? *record*)
         (let ((left-tree (interior->left-tree *record*))
               (right-tree (interior->right-tree *record*)))
           (+ (leaf-sum left-tree) (leaf-sum right-tree))))
        (else (error "leaf-sum: Invalid tree" tree))))))

;;; Anonymous figure : page 84, 85, 86

(define-record lit (datum))
(define-record varref (var))
(define-record lambda (formal body))
(define-record app (rator rand))

(define parse
  (lambda (datum)
    (cond
      ((number? datum) (make-lit datum))
      ((symbol? datum) (make-varref datum))
      ((pair? datum)
       (if (eq? (car datum) 'lambda)
           (make-lambda (caadr datum) (parse (caddr datum)))
           (make-app (parse (car datum)) (parse (cadr datum)))))
      (else (error "parse: Invalid concrete syntax" datum)))))

(define unparse
  (lambda (exp)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) var)
      (lambda (formal body) 
        (list 'lambda (list formal) (unparse body)))
      (app (rator rand) (list (unparse rator) (unparse rand)))
      (else (error "unparse: Invalid abstract syntax" exp)))))

(define free-vars
  (lambda (exp)
    (variant-case exp
      (lit (datum) '())
      (varref (var) (list var))
      (lambda (formal body) (remove formal (free-vars body)))
      (app (rator rand) 
        (union (free-vars rator) (free-vars rand))))))

;;; Anonymous figure : page 87, 88

(define-record lex-info (var distance position))

(define-record leaf (number))

(define make-leaf
  (lambda (number)
    (vector 'leaf number)))

(define leaf?
  (lambda (obj)
    (and (vector? obj)
         (= (vector-length obj) 2)
         (eq? (vector-ref obj 0) 'leaf))))

(define leaf->number
  (lambda (obj)
    (if (leaf? obj)
        (vector-ref obj 1)
        (error "leaf->number: Invalid record" obj))))

(define-record interior (symbol left-tree right-tree))

;;; Anonymous figure : page 92

(define create-empty-ff
  (lambda ()
    (lambda (symbol) 
      (error "empty-ff: no association for symbol" symbol))))

(define extend-ff
  (lambda (sym val ff)
    (lambda (symbol)
      (if (eq? symbol sym) 
          val 
          (apply-ff ff symbol)))))

(define apply-ff
  (lambda (ff symbol) 
    (ff symbol)))

;;; Anonymous figure : page 93

(define-record empty-ff ())
(define-record extended-ff (sym val ff))

(define create-empty-ff
  (lambda ()
    (make-empty-ff)))

(define extend-ff
  (lambda (sym val ff)
    (make-extended-ff sym val ff)))

(define apply-ff
  (lambda (ff symbol)
    (variant-case ff
      (empty-ff ()
        (error "empty-ff: no association for symbol" symbol))
      (extended-ff (sym val ff)
        (if (eq? symbol sym) 
            val 
            (apply-ff ff symbol)))
      (else (error "apply-ff: Invalid finite function" ff)))))

(define create-empty-ff make-empty-ff)
(define extend-ff make-extended-ff)

;;; Anonymous figure : page 94, 95

(define extend-ff*
  (lambda (sym-list val-list ff)
    (if (null? sym-list)
        ff
        (extend-ff (car sym-list) (car val-list)
          (extend-ff* (cdr sym-list) (cdr val-list) ff)))))

(define extend-ff*
  (lambda (sym-list val-list ff)
    (lambda (symbol)
      (let ((val (ribassoc symbol 
                   sym-list (list->vector val-list) '*fail*)))
        (if (eq? val '*fail*)
            (apply-ff ff symbol)
            val)))))

(define extend-ff*
  (lambda (sym-list val-list ff)
    (let ((val-vector (list->vector val-list)))
      (lambda (symbol)
        (let ((val (ribassoc symbol sym-list val-vector '*fail*)))
          (if (eq? val '*fail*)
              (apply-ff ff symbol)
              val))))))

(define-record extended-ff* (sym-list val-vector ff))

(define extend-ff*
  (lambda (sym-list val-list ff)
    (make-extended-ff* sym-list (list->vector val-list) ff)))

;;; Anonymous figure : page 96

(define apply-ff
  (lambda (ff symbol)
    (variant-case ff
      (empty-ff ()
        (error "empty-ff: no association for symbol" symbol))
      (extended-ff (sym val ff)
        (if (eq? symbol sym) 
            val 
            (apply-ff ff symbol)))
      (extended-ff* (sym-list val-vector ff)
        (let ((val (ribassoc symbol sym-list val-vector '*fail*)))
          (if (eq? val '*fail*)
              (apply-ff ff symbol)
              val)))
      (else (error "apply-ff: Invalid finite function" ff)))))

;;; End of figures for Chapter 3

;;; ****************************************************************

;;; Beginning of figures for Chapter 4

;;; Figure 4.3.2 : page 112

(define answer?
  (lambda (exp)
    (not (app? exp))))

(define reduce-once-appl
  (lambda (exp succeed fail)
    (variant-case exp
      (lit (datum) (fail))
      (varref (var) (fail))
      (lambda (formal body) (fail))
      (app (rator rand)
        (if (and (beta-redex? exp) (answer? rand))
            (succeed
              (beta-reduce exp))
            (reduce-once-appl rator
              (lambda (reduced-rator)
                (succeed
                  (make-app reduced-rator rand)))
              (lambda ()
                (reduce-once-appl rand
                  (lambda (reduced-rand)
                    (succeed
                      (make-app rator reduced-rand)))
                  fail))))))))

;;; Anonymous figure : page 119, 120

(define for-each
  (lambda (proc lst)
    (if (null? lst)
        'done
        (begin 
          (proc (car lst))
          (for-each proc (cdr lst))))))

(define displayln
  (lambda lst
    (begin
      (for-each display lst)
      (newline))))

;;; Anonymous figure : page 121, 122, 123

(define fact
  (lambda (n)
    (displayln "Entering fact with n = " n)
    (let ((ans (if (zero? n)
                   1
                   (* n (fact (- n 1))))))
      (displayln "Returning from fact with " ans)
      ans)))

(define read-eval-print
  (lambda ()
    (display "--> ")
    (write (eval (read)))
    (newline)
    (read-eval-print)))

(define reverse!
  (letrec ((loop
             (lambda (last ls)
               (let ((next (cdr ls)))
                 (set-cdr! ls last)
                 (if (null? next)
                     ls
                     (loop ls next))))))
    (lambda (ls)                     
      (if (null? ls)
          ls
          (loop '() ls)))))

;;; Figure 4.5.1 : page 125

(define cell-tag "cell")

(define make-cell
  (lambda (x)
    (vector cell-tag x)))

(define cell?
  (lambda (x)
    (and (vector? x)
         (= (vector-length x) 2)
         (eq? (vector-ref x 0) cell-tag))))

(define cell-ref
  (lambda (x)
    (if (cell? x)
        (vector-ref x 1)
        (error "Invalid argument to cell-ref:" x))))

(define cell-set!
  (lambda (x value)
    (if (cell? x) 
        (vector-set! x 1 value)
        (error "Invalid argument to cell-set!:" x))))

(define cell-swap!
  (lambda (cell-1 cell-2)
    (let ((temp (cell-ref cell-1)))
      (cell-set! cell-1 (cell-ref cell-2))
      (cell-set! cell-2 temp))))

;;; Anonymous figure : page 127

(define empty? '*)
(define push! '*)
(define pop! '*)
(define top '*)
(let ((stk '()))
  (set! empty?
    (lambda ()
      (null? stk)))
  (set! push!
    (lambda (x)
      (set! stk (cons x stk))))
  (set! pop!
    (lambda ()
      (if (empty?)
          (error "Stack empty")
          (set! stk (cdr stk)))))    
  (set! top
    (lambda ()
      (if (empty?)
          (error "Stack empty")
          (car stk)))))

;;; Figure 4.6.1 : page 129

(define stack
  (let ((stk '()))
    (lambda (message)
      (case message
        ((empty?) (lambda ()
                    (null? stk)))
        ((push!) (lambda (x)
                   (set! stk (cons x stk))))
        ((pop!) (lambda ()
                  (if (null? stk)
                      (error "Stack empty")
                      (set! stk (cdr stk)))))
        ((top) (lambda ()
                 (if (null? stk)
                     (error "Stack empty")
                     (car stk))))
        (else (error "stack: Invalid message" message))))))

;;; Figure 4.6.2 : page 130

(define make-stack
  (lambda ()
    (let ((stk '()))
      (lambda (message)
        (case message
          ((empty?) (lambda ()
                      (null? stk)))
          ((push!) (lambda (x)
                     (set! stk (cons x stk))))
          ((pop!) (lambda ()
                    (if (null? stk)
                        (error "Stack empty")
                        (set! stk (cdr stk)))))
          ((top) (lambda ()
                   (if (null? stk)
                       (error "Stack empty")
                       (car stk))))
          (else (error "stack: Invalid message" message)))))))

;;; Exericse 4.6.3 : page 130

(define next-symbol
  (let ((c 0))
    (lambda ()
      (set! c (+ c 1))
      (string->symbol (string-append "g" (number->string c))))))

;;; Anonymous figure : page 132, 133, 134, 135, 136

(define stream-for-each
  (lambda (proc stream)
    (letrec ((loop (lambda (stream)
                     (if (not (stream-null? stream))
                         (begin
                           (proc (stream-car stream))
                           (loop (stream-cdr stream)))))))
      (loop stream))))

(define display-stream
  (lambda (stream)
    (stream-for-each display stream)
    (newline)))

(define string->stream
  (lambda (string)
    (let ((string-len (string-length string)))
      (letrec
        ((loop (lambda (cursor)
                 (if (= cursor string-len)
                     the-null-stream
                     (make-stream
                       (string-ref string cursor)
                       (lambda ()
                         (loop (+ cursor 1))))))))
        (loop 0)))))

(define stream->list
  (lambda (stream)
    (if (stream-null? stream)
        '()
        (cons (stream-car stream) 
          (stream->list (stream-cdr stream))))))

(define stream-filter
  (lambda (pred stream)
    (cond
      ((stream-null? stream) the-null-stream)
      ((pred (stream-car stream))
       (make-stream (stream-car stream)
         (lambda ()
           (stream-filter pred (stream-cdr stream)))))
      (else (stream-filter pred (stream-cdr stream))))))

(define make-input-stream
  (lambda ()
    (let ((char (read-char)))
      (if (eof-object? char)
          the-null-stream
          (make-stream char make-input-stream)))))

(define read-print
  (lambda ()
    (display "--> ")
    (display-stream (make-input-stream))
    (read-print)))

(define stream-car car)

(define stream-cdr cdr)

(define make-stream
  (lambda (value thunk)
    (cons value (thunk))))

(define the-null-stream '())

(define stream-null? 
  (lambda (stream)
    (eq? stream the-null-stream)))

(define stream-cdr
  (lambda (stream)
    ((cdr stream))))

(define make-stream
  (lambda (value thunk)
    (cons value thunk)))

(define the-null-stream 
  (make-stream "end-of-stream" (lambda () the-null-stream)))

(define stream-cdr
  (lambda (stream)
    (if (pair? (cdr stream))
        (cdr stream)
        (let ((s ((cdr stream))))
          (set-cdr! stream s)
          s))))

;;; End of figures for Chapter 4

;;; ****************************************************************

;;; Beginning of figures for Chapter 5

;;; Anonymous figure : page 140

(define-record lit (datum))
(define-record varref (var))
(define-record app (rator rands))

;;; Figure 5.1.1 : page 141

(define eval-exp
  (lambda (exp)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (apply-env init-env var))
      (app (rator rands)
        (let ((proc (eval-exp rator))
              (args (eval-rands rands)))
          (apply-proc proc args)))
      (else (error "Invalid abstract syntax: " exp)))))

(define eval-rands
  (lambda (rands)
    (map eval-exp rands)))

;;; Anonymous figure : page 142, 143

(define the-empty-env (create-empty-ff))
(define extend-env extend-ff*)
(define apply-env apply-ff)

(define-record prim-proc (prim-op))

(define apply-proc
  (lambda (proc args)
    (variant-case proc
      (prim-proc (prim-op) (apply-prim-op prim-op args))
      (else (error "Invalid procedure:" proc)))))

(define apply-prim-op
  (lambda (prim-op args)
    (case prim-op
      ((+) (+ (car args) (cadr args)))
      ((-) (- (car args) (cadr args)))
      ((*) (* (car args) (cadr args)))
      ((add1) (+ (car args) 1))
      ((sub1) (- (car args) 1))
      (else (error "Invalid prim-op name:" prim-op)))))

(define prim-op-names '(+ - * add1 sub1))

(define init-env 
  (extend-env
    prim-op-names
    (map make-prim-proc prim-op-names)
    the-empty-env))

(define run
  (lambda (x)
    (eval-exp (parse x))))

(define read-eval-print
  (lambda ()
    (display "-->")
    (write (eval-exp (parse (read))))
    (newline)
    (read-eval-print)))

;;; Anonymous figure : page 146

(define true-value?
  (lambda (x)
    (not (zero? x))))

;;; Anonymous figure : page 148

(define-record let (decls body))
(define-record decl (var exp))

;;; Figure 5.3.1 : page 149

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (apply-env env var))
      (app (rator rands)
        (let ((proc (eval-exp rator env))
              (args (eval-rands rands env)))
          (apply-proc proc args)))
      (if (test-exp then-exp else-exp)
        (if (true-value? (eval-exp test-exp env))
            (eval-exp then-exp env)
            (eval-exp else-exp env)))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (rand) (eval-exp rand env))
         rands)))

;;; Figure 5.3.2 : page 150

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (apply-env env var))
      (app (rator rands)
        (let ((proc (eval-exp rator env))
              (args (eval-rands rands env)))
          (apply-proc proc args)))
      (if (test-exp then-exp else-exp)
        (if (true-value? (eval-exp test-exp env))
            (eval-exp then-exp env)
            (eval-exp else-exp env)))
      (let (decls body)
        (let ((vars (map decl->var decls))
              (exps (map decl->exp decls)))
          (let ((new-env (extend-env vars
                           (eval-rands exps env) 
                           env)))
            (eval-exp body new-env))))
      (else (error "Invalid abstract syntax:" exp)))))

;;; Anonymous figure : page 152, 153

(define-record closure (formals body env))

(define apply-proc
  (lambda (proc args)
    (variant-case proc
      (prim-proc (prim-op) (apply-prim-op prim-op args))
      (closure (formals body env)
        (eval-exp body (extend-env formals args env)))
      (else (error "Invalid procedure:" proc)))))

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (proc (formals body) 
        (make-closure formals body env))
      (else ...))))

;;; Figure 5.4.1 : page 154

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (apply-env env var))
      (app (rator rands)
        (let ((proc (eval-exp rator env))
              (args (eval-rands rands env)))
          (apply-proc proc args)))
      (if (test-exp then-exp else-exp)
        (if (true-value? (eval-exp test-exp env))
            (eval-exp then-exp env)
            (eval-exp else-exp env)))
      (let (decls body)
        (let ((vars (map decl->var decls))
              (exps (map decl->exp decls)))
          (let ((new-env (extend-env vars
                           (eval-rands exps env)
                           env)))
            (eval-exp body new-env))))
      (proc (formals body)
        (make-closure formals body env))
      (else (error "Invalid abstract syntax:" exp)))))

(define apply-proc
  (lambda (proc args)
    (variant-case proc
      (prim-proc (prim-op) (apply-prim-op prim-op args))
      (closure (formals body env)
        (eval-exp body (extend-env formals args env)))
      (else (error "Invalid procedure:" proc)))))

;;; Anonymous figure : page 156

(define-record varassign (var exp))

;;; Figure 5.5.1 : page 157

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (cell-ref (apply-env env var)))
      (app (rator rands)
        (let ((proc (eval-exp rator env))
              (args (eval-rands rands env)))
          (apply-proc proc args)))
      (if (test-exp then-exp else-exp)
        (if (true-value? (eval-exp test-exp env))
            (eval-exp then-exp env)
            (eval-exp else-exp env)))
      (proc (formals body) 
        (make-closure formals body env))
      (varassign (var exp)
        (cell-set! (apply-env env var) (eval-exp exp env)))
      (else (error "Invalid abstract syntax: " exp)))))

(define apply-proc
  (lambda (proc args)
    (variant-case proc
      (prim-proc (prim-op) (apply-prim-op prim-op args))
      (closure (formals body env)
        (eval-exp body 
          (extend-env
            formals
            (map make-cell args)
            env)))
      (else (error "Invalid procedure:" proc)))))

;;; Exercise 5.5.3 : page 158

(define denoted->expressed cell-ref)

(define apply-proc
  (lambda (proc args)
    (variant-case proc
      (prim-proc (prim-op)
        (apply-prim-op prim-op (map denoted->expressed args)))
      (closure (formals body env)
        (eval-exp body
          (extend-env formals args env)))
      (else (error "Invalid procedure:" proc)))))

;;; Exercise 5.5.9 : page 161, 162

(define eval-exp
  (lambda (exp env store)
    (variant-case exp
      (varref (var) (apply-store store (apply-env env var)))
      (else ...))))

(define-record interp-result (value store))

(define eval-rands
  (lambda (rands env store)
    (letrec
      ((loop
         (lambda (rands ans store)
           (if (null? rands)
               (make-interp-result (reverse ans) store)
               (let ((first-result 
                       (eval-exp (car rands) env store)))
                 (loop
                   (cdr rands)
                   (cons (interp-result->value first-result) ans)
                   (interp-result->store first-result)))))))
      (loop rands '() store))))

;;; Figure 5.6.2 : page 165

(define-record extended-rec-env (vars vals old-env))

(define extend-rec-env  ; corrected for second printing
  (lambda (procdecls env)
    (make-extended-rec-env
      (map procdecl->var procdecls)
      (list->vector
        (map (lambda (procdecl)
               (make-proc
                 (procdecl->formals procdecl)
                 (procdecl->body procdecl)))
             procdecls))
      env)))

(define apply-env  ; corrected for second printing
  (lambda (env var)
    (variant-case env
      (extended-rec-env (vars vals old-env)
        (let ((p (ribassoc var vars vals '*fail*)))
          (if (eq? p '*fail*)
              (apply-env old-env var)
              (make-closure (proc->formals p) (proc->body p) env))))
      (else ...))))

;;; Figure 5.7.1 : page 169

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (cell-ref (apply-env env var)))
      (app (rator rands)
        (let ((proc (eval-exp rator env))
              (args (eval-rands rands env)))
          (apply-proc proc args env)))
      (if (test-exp then-exp else-exp)
        (if (true-value? (eval-exp test-exp env))
            (eval-exp then-exp env)
            (eval-exp else-exp env)))
      (proc (formals body) exp)
      (varassign (var exp)
        (cell-set! (apply-env env var) (eval-exp exp env)))
      (else (error "Invalid abstract syntax:" exp)))))

(define apply-proc
  (lambda (proc args current-env)
    (variant-case proc
      (prim-proc (prim-op) (apply-prim-op prim-op args))
      (proc (formals body)
        (eval-exp body 
          (extend-env formals (map make-cell args) current-env)))
      (else (error "Invalid procedure:" proc)))))

;;; Figure 5.7.2 : page 171

(define eval-exp
  (lambda (exp)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (cell-ref (lookup-in-env var)))
      (app (rator rands)
        (let ((proc (eval-exp rator))
              (args (eval-rands rands)))
          (apply-proc proc args)))
      (if (test-exp then-exp else-exp)
        (if (true-value? (eval-exp test-exp))
            (eval-exp then-exp)
            (eval-exp else-exp)))
      (proc (formals body) exp)
      (varassign (var exp)
        (cell-set! (lookup-in-env var) (eval-exp exp)))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands)
    (map (lambda (exp) (eval-exp exp)) 
         rands)))

(define apply-proc
  (lambda (proc args)
    (variant-case proc
      (prim-proc (prim-op) (apply-prim-op prim-op args))
      (proc (formals body)
        (push-env! formals (map make-cell args))
        (let ((value (eval-exp body)))
          (pop-env!)
          value))
      (else (error "Invalid procedure:" proc)))))

;;; End of figures for Chapter 5

;;; ****************************************************************

;;; Beginning of figures for Chapter 6

;;; Figure 6.1.1 : page 181

(define-record aggregate (vector))

(define make-array
  (lambda (length)
    (make-aggregate (make-vector length))))

(define array? aggregate?)

(define array-ref
  (lambda (array index)
    (vector-ref (aggregate->vector array) index)))

(define array-set!
  (lambda (array index value)
    (vector-set! (aggregate->vector array) index value)))

(define array-copy
  (lambda (array)
    (make-aggregate (list->vector (vector->list (aggregate->vector array))))))

;;; Figure 6.1.1 : page 181 %%%%% New array ADT for new direct arrays %%%%%%

(define make-array
  (lambda (dimension)
    (let ((array (make-vector (+ dimension 1))))
      (vector-set! array 0 '*array*)
      array)))

(define array?
  (lambda (x)
    (and (vector? x) (eq? (vector-ref x 0) '*array*))))

(define array-ref
  (lambda (array index)
    (vector-ref array (+ index 1))))

(define array-set!
  (lambda (array index value)
    (vector-set! array (+ index 1) value)))

(define array-whole-set!
  (lambda (dest-array source-array)
    (let ((source-len (vector-length source-array)))
      (letrec 
        ((loop (lambda (n)
                 (if (< n source-len)
                     (begin
                       (vector-set! dest-array n (vector-ref source-array n))
                       (loop (+ n 1)))))))
        (loop 1)))))

(define array-copy
  (lambda (array)
    (let ((new-array (make-array (- (vector-length array) 1))))
      (array-whole-set! new-array array)
      new-array)))

;;; Figure 6.1.2 : page 182

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (varref (var) (denoted->expressed (apply-env env var)))
      (app (rator rands) 
        (apply-proc (eval-rator rator env) (eval-rands rands env)))
      (varassign (var exp)
        (denoted-value-assign! (apply-env env var) (eval-exp exp env)))
      (letarray (arraydecls body)
        (eval-exp body 
          (extend-env (map decl->var arraydecls)
            (map (lambda (decl)
                   (do-letarray (eval-exp (decl->exp decl) env)))
                 arraydecls)
            env)))
      (arrayref (array index) 
        (array-ref (eval-array-exp array env)
          (eval-exp index env)))
      (arrayassign (array index exp)
        (array-set! (eval-array-exp array env)
          (eval-exp index env) 
          (eval-exp exp env)))
      (else ...))))

(define eval-rator
  (lambda (rator env)
    (eval-exp rator env)))

(define eval-rands
  (lambda (rands env)
    (map (lambda (rand) (eval-rand rand env)) rands)))

(define eval-rand
  (lambda (exp env)
    (expressed->denoted (eval-exp exp env))))

(define apply-proc
  (lambda (proc args)
    (variant-case proc
      (prim-proc (prim-op) (apply-prim-op prim-op (map denoted->expressed args)))
      (closure (formals body env) (eval-exp body (extend-env formals args env)))
      (else (error "Invalid procedure:" proc)))))

;;; Figure 6.1.3 : page 183

(define denoted->expressed cell-ref)

(define denoted-value-assign! cell-set!)

(define do-letarray (compose make-cell make-array))

(define eval-array-exp eval-exp)

(define expressed->denoted make-cell)

;;; Figure 6.1.4 : page 184

(define denoted->expressed
  (lambda (den-val)
    (let ((exp-val (cell-ref den-val)))
      (if (array? exp-val) (array-copy exp-val) exp-val))))

(define eval-array-exp
  (lambda (exp env)
    (variant-case exp
      (varref (var) (let ((exp-val (cell-ref (apply-env env var))))
                      (if (array? exp-val)
                          exp-val
                          (error "Expecting an array:" exp-val))))
      (else (eval-exp exp env)))))

;;; Figure 6.1.4 : page 184 %%%%%% New direct arrays %%%%%%

(define denoted->expressed
  (lambda (den-val)
    (if (array? den-val)
        den-val
        (cell-ref den-val))))

(define denoted-value-assign!
  (lambda (den-val exp-val)
    (cond
      ((not (array? den-val)) (cell-set! den-val exp-val))
      ((array? exp-val) (array-whole-set! den-val exp-val))
      (else (error "Must assign array:" den-val)))))

(define do-letarray make-array)

(define eval-array-exp eval-exp)

(define expressed->denoted
  (lambda (exp-val)
    (if (array? exp-val)
        (array-copy exp-val)
        (make-cell exp-val))))

;;; Anonymous figure : page 184 %%%%%% New direct arrays %%%%%%

(define array-set!
  (lambda (array index value)
    (if (array? value)
        (error "Cannot assign array to array element" value)
        (vector-set! array (+ index 1) value))))

;;; Anonymous figure : page 189

(define eval-rand
  (lambda (rand env)
    (variant-case rand
      (varref (var) (apply-env env var))
      (else (error "Invalid operand:" rand)))))

(define-record ae (array index))

;;; Figure 6.2.1 : page 191

(define eval-rand
  (lambda (rand env)
    (variant-case rand
      (varref (var) (apply-env env var))
      (arrayref (array index)
        (make-ae (eval-array-exp array env) (eval-exp index env)))
      (else (make-cell (eval-exp rand env))))))

(define denoted->expressed
  (lambda (den-val)
    (cond
      ((cell? den-val) (cell-ref den-val))
      ((ae? den-val) (array-ref (ae->array den-val) (ae->index den-val)))
      (else (error "Can't dereference denoted value:" den-val)))))

(define denoted-value-assign!
  (lambda (den-val val)
    (cond
      ((cell? den-val) (cell-set! den-val val))
      ((ae? den-val) (array-set! (ae->array den-val) (ae->index den-val) val))
      (else (error "Can't assign to denoted value:" den-val)))))

;;; Figure 6.4.1 : page 197

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (letproc (procdecls body)
        (let ((vars (map procdecl->var procdecls))
              (closures (map (lambda (decl)
                               (make-closure
                                 (procdecl->formals decl)
                                 (procdecl->body decl)
                                 env))
                              procdecls)))
          (let ((new-env (extend-env vars closures env)))
            (eval-exp body new-env))))
      (local (decls body)
        (let ((vars (map decl->var decls))
              (exps (map decl->exp decls)))
          (let ((new-env (extend-env vars
                           (map (lambda (exp) 
                                  (make-cell (eval-exp exp env)))
                                exps)
                           env)))
            (eval-exp body new-env))))
      (else ...))))

(define eval-rator
  (lambda (rator env)
    (let ((den-val (apply-env env (varref->var rator))))
      (if (closure? den-val)
          den-val
          (denoted->expressed den-val)))))

;;; Figure 6.4.2 : page 198

(define denoted->expressed
  (lambda (den-val)
    (cond
      ((cell? den-val) (cell-ref den-val))
      ((ae? den-val) (array-ref (ae->array den-val) (ae->index den-val)))
      (else (error "Can't dereference denoted value:" den-val)))))

(define denoted-value-assign!
  (lambda (den-val val)
    (cond
      ((cell? den-val) (cell-set! den-val val))
      ((ae? den-val) (array-set! (ae->array den-val) (ae->index den-val) val))
      (else (error "Can't assign to denoted value:" den-val)))))

(define do-letarray make-array)  

(define eval-array-exp
  (lambda (array-exp env)
    (apply-env env (varref->var array-exp))))

;;; Anonymous figure : page 202

(define-record thunk (exp env))

;;; Figure 6.5.1 : page 203

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (local (decls body)
        (let ((vars (map decl->var decls))
              (exps (map decl->exp decls)))
          (let ((new-env (extend-env vars
                           (map (lambda (exp)
                                  (make-cell (eval-exp exp env)))
                                exps)
                           env)))
            (eval-exp body new-env))))
      (app (rator rands)
        (apply-proc (eval-rator rator env) (eval-rands rands env)))
      (varref (var) (denoted->expressed (apply-env env var)))
      (varassign (var exp)
        (denoted-value-assign! (apply-env env var) (eval-exp exp env)))
      (letarray (arraydecls body)
        (eval-exp body 
          (extend-env (map decl->var arraydecls)
            (map (lambda (decl)
                   (do-letarray (eval-exp (decl->exp decl) env)))
                 arraydecls)
            env)))
      (arrayref (array index) 
        (array-ref (eval-array-exp array env) (eval-exp index env)))
      (arrayassign (array index exp)
        (array-set!
          (eval-array-exp array env)
          (eval-exp index env)
          (eval-exp exp env)))
      (else ...))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (rand) (make-thunk rand env)) rands)))

;;; Figure 6.5.2 : page 204

(define eval-rand
  (lambda (rand env)
    (variant-case rand
      (varref (var) (denoted->L-value (apply-env env var)))
      (arrayref (array index)
        (make-ae (eval-array-exp array env) (eval-exp index env)))
      (else (make-cell (eval-exp rand env))))))

(define denoted->L-value
  (lambda (den-val)
    (if (thunk? den-val)
        (eval-rand (thunk->exp den-val) (thunk->env den-val))
        den-val)))

(define denoted->expressed
  (lambda (den-val)
    (let ((l-val (denoted->L-value den-val)))
      (cond
        ((cell? l-val) (cell-ref l-val))
        ((ae? l-val)
         (array-ref (ae->array l-val) (ae->index l-val)))
        (else (error "Can't dereference denoted value:" l-val))))))

(define denoted-value-assign!
  (lambda (den-val exp-val)
    (let ((l-val (denoted->L-value den-val)))
      (cond
        ((cell? l-val) (cell-set! l-val exp-val))
        ((ae? l-val) (array-set! (ae->array l-val) (ae->index l-val) exp-val))
        (else (error "Can't assign to denoted value:" l-val))))))

(define expressed->denoted make-cell)

(define do-letarray (compose make-cell make-array))

(define eval-array-exp eval-exp)

(define eval-rator eval-exp)

;;; Anonymous figure : page 207

(define-record memo (cell))

;;; Figure 6.5.3 : page 208

(define eval-rands
  (lambda (rands env)
    (map (lambda (rand)
           (make-memo (make-cell (make-thunk rand env))))
         rands)))

(define denoted->L-value
  (lambda (den-val)
    (if (memo? den-val)
        (let ((cell (memo->cell den-val)))
          (let ((contents (cell-ref cell)))
            (if (thunk? contents)
                (let ((l-val (eval-rand 
                               (thunk->exp contents)
                               (thunk->env contents))))
                  (cell-set! cell l-val)
                  l-val)
                contents)))
        den-val)))

;;; End of figures for Chapter 6

;;; ****************************************************************

;;; Beginning of figures for Chapter 7

;;; Figure 7.1.1 : page 215

(define make-stack
  (let ((pushed 0))
    (lambda ()
      (let ((stk '()) (local-pushed 0))
        (lambda (message)
          (case message
            ((empty?) (lambda () (null? stk)))
            ((push!) (lambda (x)
                       (set! pushed (+ pushed 1))
                       (set! local-pushed (+ local-pushed 1))
                       (set! stk (cons x stk))))
            ((pop!) (lambda ()
                      (if (null? stk)
                          (error "Stack: Underflow")
                          (begin
                            (set! pushed (- pushed 1))
                            (set! local-pushed (- local-pushed 1))
                            (set! stk (cdr stk))))))
            ((top) (lambda ()
                     (if (null? stk)
                         (error "Stack: Underflow")
                         (car stk))))
            ((local-pushed) (lambda () local-pushed))
            ((pushed) (lambda () pushed))
            (else (error "Stack: Invalid message" message))))))))

;;; Anonymous figure : page 218

(define-record instance (class i-vals))
(define-record class (c-vars c-vals i-vars m-env))

;;; Figure 7.1.3 : page 219

(define eval-exp
  (lambda (exp env class inst)
    (variant-case exp
      (i-varref (var) (lookup var (class->i-vars class) (instance->i-vals inst)))
      (c-varref (var) (lookup var (class->c-vars class) (class->c-vals class)))
      (i-varassign (var exp)
        (let ((value (eval-exp exp env class inst)))
          (assign var value (class->i-vars class) (instance->i-vals inst))))
      (c-varassign (var exp)
        (let ((value (eval-exp exp env class inst))
              (c-vals (class->c-vals class)))
          (assign var value (class->c-vars class) c-vals)))
      (method (formals body)
        (let ((new-formals (cons 'self formals)))
          (lambda (class-thunk)
            (lambda (args)
              (eval-exp body (extend-env new-formals (map make-cell args) env)
                (class-thunk)
                (car args))))))
      (meth-app (name rands)
        (let ((args (map (lambda (rand) (eval-exp rand env class inst)) rands)))
          (meth-call name (instance->class (car args)) args)))
      (new-simpleinst (class-exp)
        (let ((inst-class (eval-exp class-exp env class inst)))
          (let ((new-inst (make-instance inst-class
                            (make-vals (class->i-vars inst-class)))))
            (meth-call 'initialize inst-class (list new-inst))
            new-inst)))      
      (new-simpleclass (c-vars i-vars methdecls init-exp)
        (let ((open-methods
                (map (lambda (decl) (eval-exp (decl->exp decl) env class inst))
                     methdecls)))
          (letrec ((new-class
                     (make-class c-vars (make-vals c-vars) i-vars
                       (extend-env (map decl->var methdecls)
                         (map (lambda (open-meth) (open-meth (lambda () new-class)))
                              open-methods)
                         init-meth-env))))
            (eval-exp init-exp env new-class (make-instance new-class '#()))
            new-class)))
      (else '...))))

;;; Figure 7.1.4 : page 220

(define lookup
  (lambda (var vars vals)
    (cell-ref (cell-lookup var vars vals))))

(define assign
  (lambda (var value vars vals)
    (cell-set! (cell-lookup var vars vals) value)))

(define cell-lookup
  (lambda (var vars vals)
    (letrec ((loop (lambda (vars c)
                     (cond
                       ((null? vars) (error "Unassigned variable:" var))
                       ((eq? (car vars) var) (vector-ref vals c))
                       (else (loop (cdr vars) (- c 1)))))))
      (loop vars (- (length vars) 1)))))

(define meth-call
  (lambda (name class args)
    (let ((method (meth-lookup name class)))
      (method args))))

(define meth-lookup
  (lambda (name class)
    (apply-env (class->m-env class) name))) 

(define make-vals
  (lambda (vars)
    (list->vector (map (lambda (x) (make-cell '*)) vars))))

(define init-meth-env ; corrected for second printing
  (extend-env 
    '(initialize)
    (list (lambda (args) "Not initialized"))
    the-empty-env))

(define base-env init-env) ; init-env of Chapter 5

(define init-env
  (extend-env '(classof)
    (list (make-cell (make-prim-proc 'instance->class)))
    base-env))

;;; Figure 7.2.1 : page 225

(define make-bounded-stack
  (lambda ()
    (let ((bound 10) (stack (make-stack)))
      (lambda (message)
        (case message
          ((push!) (if (< ((stack 'local-pushed)) bound)
                       (stack 'push!)
                       (error "Bounded stack: bound exceeded")))
          ((set-bound!) (lambda (x) (set! bound x)))
          (else (stack message)))))))

;;; Anonymous figure : page 227

(define-record class (parent c-vars c-vals i-vars m-env))

;;; Figure 7.2.4 : page 228

(define eval-exp
  (lambda (exp env class inst)
    (variant-case exp
      (new-class (parent-exp c-vars i-vars methdecls init-exp)
        (let ((parent-class (eval-exp parent-exp env class inst))
              (open-methods (map (lambda (decl)
                                   (eval-exp (decl->exp decl) env class inst))
			      methdecls)))
          (let ((new-c-vars (append c-vars (class->c-vars parent-class)))
                (new-i-vars (append i-vars (class->i-vars parent-class)))
		(new-c-vals
		  (make-shared-c-vals (class->c-vals parent-class) c-vars)))
	    (letrec ((new-class
		       (make-class parent-class new-c-vars new-c-vals
			 new-i-vars
			 (extend-env (map decl->var methdecls)
			   (map (lambda (open-method)
				  (open-method (lambda () new-class)))
			     open-methods)
			   (class->m-env parent-class)))))
	      (eval-exp init-exp env new-class (make-instance new-class '#()))
	      new-class))))
      (super-meth-app (name rands)
        (let ((args (map (lambda (x) (eval-exp x env class inst)) rands)))
          (meth-call name (class->parent class) (cons inst args))))
      (else '...))))

(define make-shared-c-vals
  (lambda (parent-c-vals c-vars)
    (list->vector
      (append
        (vector->list parent-c-vals)
        (map (lambda (x) (make-cell '*)) c-vars)))))

(define init-env
  (extend-env '(baseobject parentof classof)
    (map make-cell
         (list (make-class '* '() '#() '() init-meth-env)
	       (make-prim-proc 'class->parent)
               (make-prim-proc 'instance->class)))
    base-env))

;;; Anonymous figure : page 233

(define-record instance (class parent i-vars m-env i-vals))

(define class->m-env instance->m-env)
(define class->i-vars instance->i-vars)
(define class->parent instance->parent)

;;; Figure 7.3.1 : page 234

(define eval-exp
  (lambda (exp env class inst)
    (variant-case exp
      (c-varref (var) 
	(lookup var (class->i-vars (instance->class class))
	  (instance->i-vals (instance->class inst))))
      (c-varassign (var exp)
	(let ((value (eval-exp exp env class inst)))
	  (assign var value (class->i-vars (instance->class class))
	    (instance->i-vals (instance->class inst)))))
      (new-instance (class-exp parent-exp i-vars methdecls)
	(let ((inst-class (eval-exp class-exp env class inst))
	      (parent-class (eval-exp parent-exp env class inst))
	      (open-methods
		(map (lambda (decl)
		       (eval-exp (decl->exp decl) env class inst))
		  methdecls)))
	  (let ((new-i-vars (append i-vars (class->i-vars parent-class))))
	    (letrec ((new-inst
		       (make-instance inst-class parent-class new-i-vars
			 (extend-env (map decl->var methdecls)
			   (map (lambda (open-method)
				  (open-method (lambda () new-inst)))
			     open-methods)
			   (class->m-env parent-class))
			 (make-vals (class->i-vars inst-class)))))
	      (meth-call 'initialize inst-class (list new-inst))
	      new-inst))))
      (else ...))))

(define init-env
  (extend-env '(baseobject parentof classof)
    (map make-cell
         (list (make-instance '* '* '() init-meth-env '#())
	       (make-prim-proc 'class->parent)
               (make-prim-proc 'instance->class)))
    base-env))

;;; End of figures for Chapter 7

;;; ****************************************************************

;;; Beginning of figures for Chapter 8

;;; Anonymous figure : page 243

(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))

(define fact-iter
  (lambda (n)
    (fact-iter-acc n 1)))

(define fact-iter-acc
  (lambda (n a)
    (if (zero? n) a (fact-iter-acc (- n 1) (* n a)))))

;;; Anonymous figure : page 246, 247

(define fact-iter
  (lambda (n)
    (let ((a 1))
      (letrec ((loop (lambda ()
                       (if (zero? n)
                           a
                           (begin
                             (set! a (* n a))
                             (set! n (- n 1))
                             (loop))))))
        (loop)))))

(define revappend
  (lambda (ls1 ls2)
    (if (null? ls1)
        ls2
        (revappend (cdr ls1) (cons (car ls1) ls2)))))

;;; Exercise 8.1.3 : page 247

(define even-length?
  (lambda (ls)
    (if (null? ls) #t (odd-length? (cdr ls)))))

(define odd-length?
  (lambda (ls)
    (if (null? ls) #f (even-length? (cdr ls)))))

(define even-length?
  (lambda (ls)
    (even? (length ls))))

(define length
  (lambda (ls)
    (if (null? ls) 0 (+ 1 (length (cdr ls))))))

;;; Anonymous figure : page 248. 249, 250

(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))

(define fact-cps
  (lambda (n k)
    (if (zero? n)
        (k 1)
        (fact-cps (- n 1) 
          (lambda (v) (k (* n v)))))))

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ (length (cdr ls)) 1))))

(define length-cps
  (lambda (ls k)
    (if (null? ls)
        (k 0)
        (length-cps (cdr ls)
          (lambda (v) (k (+ v 1)))))))

;;; Anonymous figure : page 252

(define-record prim-app (primitive rands))

;;; Figure 8.3.2 : page 256

(define simple?
  (lambda (exp)
    (and (not (app? exp))
         (andmap simple? (head-exps exp))
         (andmap simple? (tail-exps exp)))))

(define head-exps
  (lambda (exp)
    (variant-case exp
      (lit (datum) '())
      (varref (var) '())
      (if (test-exp then-exp else-exp) (list test-exp))
      (proc (formals body) '())
      (let (decls body) (map decl->exp decls))
      (letrec (decls body) (map decl->exp decls))
      (prim-app (primitive rands) rands)
      (app (rator rands) (cons rator rands)))))

(define tail-exps
  (lambda (exp)
    (variant-case exp
      (lit (datum) '())
      (varref (var) '())
      (if (test-exp then-exp else-exp) (list then-exp else-exp))
      (proc (formals body) '())
      (let (decls body) (list body))
      (letrec (decls body) (list body))
      (prim-app (primitive rands) '())
      (app (rator rands) '()))))

(define binding-vars
  (lambda (exp)
    (variant-case exp
      (let (decls body) (map decl->var decls))
      (letrec (decls body) (map decl->var decls))
      (else '()))))

;;; Anonymous figure : page 257 (recently added for 3rd printing)

(define tail-form?
  (lambda (exp)
    (variant-case exp
      (proc (formals body) (tail-form? body))
      (else (and (andmap (lambda (x) 
                           (and (simple? x) (tail-form? x)))
                         (head-exps exp))
                 (andmap tail-form? (tail-exps exp)))))))

;;; Figure 8.4.1 : page 261

(define initial-exp
  (lambda (exp)
    (letrec
      ((loop (lambda (ls)
               (cond
                 ((null? ls) exp)
                 ((simple? (car ls)) (loop (cdr ls)))
                 (else (initial-exp (car ls)))))))
      (loop (head-exps exp)))))

;;; Figure 8.4.3 : page 266

(define positional-substitution
  (lambda (exp pairs)
    (letrec
      ((loop
         (lambda (exp)
           (let ((found-pair (assq exp pairs)))
             (if (pair? found-pair)
                 (cdr found-pair)
                 (variant-case exp
                   (lit (datum) exp)
                   (varref (var) exp)
                   (if (test-exp then-exp else-exp)
                     (make-if
                       (loop test-exp)
                       (loop then-exp)
                       (loop else-exp)))
                   (proc (formals body)
                     (make-proc formals (loop body)))
                   (let (decls body)
                     (make-let
                       (map (lambda (decl)
                              (make-decl
                                (decl->var decl)
                                (loop (decl->exp decl))))
                            decls)
                       (loop body)))
                   (letrec (decls body)
                     (make-letrec
                       (map (lambda (decl)
                              (make-decl
                                (decl->var decl)
                                (loop (decl->exp decl))))
                            decls)
                       (loop body)))
                   (prim-app (primitive rands)
                     (make-prim-app primitive (map loop rands)))            
                   (app (rator rands)
                     (make-app (loop rator) (map loop rands)))))))))
      (loop exp))))

;;; Figure 8.4.5 : page 270. 271

(define alpha-convert
  (lambda (exp bvs)
    (let ((table
            (let ((pairs (map (lambda (bv)
                                (cons bv (next-symbol-right bv)))
                              bvs)))
              (lambda (sym)
                (let ((found-pair (assq sym pairs)))
                  (if (pair? found-pair)
                      (cdr found-pair)
                      sym))))))
      (variant-case exp
        (let (decls body)
          (make-let
            (map (lambda (decl)
                   (make-decl
                     (table (decl->var decl))
                     (decl->exp decl)))
                 decls)
            (beta body table)))
        (letrec (decls body)
          (make-letrec
            (map
              (lambda (decl)
                (make-decl
                  (table (decl->var decl))
                  (beta (decl->exp decl) table)))
              decls)
            (beta body table)))
        (else exp)))))

(define beta
  (lambda (exp table)
    (variant-case exp
      (lit (datum) exp)
      (varref (var) (make-varref (table var)))
      (if (test-exp then-exp else-exp)
        (make-if
          (beta test-exp table)
          (beta then-exp table)
          (beta else-exp table)))
      (proc (formals body)
        (make-proc formals
          (beta body (lambda (var)
                       (if (memq var formals) var (table var))))))
      (let (decls body)
        (make-let
          (map (lambda (decl)
                 (make-decl (decl->var decl)
                   (beta (decl->exp decl) table)))
               decls)
          (beta body (let ((vars (map decl->var decls)))
                       (lambda (var)
                         (if (memq var vars) var (table var)))))))
      (letrec (decls body)
        (let ((new-table
                (let ((vars (map decl->var decls)))
                  (lambda (var)
                    (if (memq var vars) var (table var))))))
          (make-letrec
            (map (lambda (decl)
                   (make-decl (decl->var decl)
                     (beta (decl->exp decl) new-table)))
                 decls)
            (beta body new-table))))
      (prim-app (primitive rands)
        (make-prim-app primitive
          (map (lambda (rand) (beta rand table)) rands)))
      (app (rator rands)
        (make-app (beta rator table)
          (map (lambda (rand) (beta rand table)) rands))))))

;;; Anonymous figure : page 272

(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (car los))
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

;;; Anonymous figure : page 274

(define subst
  (lambda (new old slst)
    (if (null? slst)
        '()
        (if (symbol? (car slst))
            (if (eq? (car slst) old)
                (cons new (subst new old (cdr slst)))
                (cons (car slst) (subst new old (cdr slst))))
            (cons (subst new old (car slst))
                  (subst new old (cdr slst)))))))

;;; Anonymous figure : page 277

(define remove
  (lambda (s los)
    (letrec
      ((loop 
         (lambda (los)
           (if (null? los)
               '()
               (if (eq? s (car los))
                   (loop (cdr los))
                   (cons (car los) (loop (cdr los))))))))
      (loop los))))


;;; Exercise 8.5.2 : page 279

(define final-valcont
  (lambda (v)
    (display "The answer is: ")
    (write v)
    (newline)))

;;; Exercise 8.5.3 : page 279, 280

(define remove*
  (lambda (a alst)
    (cond
      ((null? alst) '())
      ((pair? (car alst))
       (cons (remove* a (car alst)) (remove* a (cdr alst))))
      ((eq? (car alst) a) (remove* a (cdr alst)))
      (else (cons (car alst) (remove* a (cdr alst)))))))

(define member*
  (lambda (a alst)
    (cond
      ((null? alst) #f)
      ((pair? (car alst))
       (or (member* a (car alst)) (member* a (cdr alst))))
      ((eq? (car alst) a) alst)
      (else (member* a (cdr alst))))))

(define remfirst*
  (lambda (a alst)
    (letrec ((loop (lambda (alst)
                     (cond
                       ((null? alst) '())
                       ((not (pair? (car alst)))
                       (if (eq? (car alst) a)
                           (cdr alst)
                           (cons (car alst) (loop (cdr alst)))))
                   ((equal? (loop (car alst)) (car alst))
                    (cons (car alst) (loop (cdr alst))))
                   (else (cons (loop (car alst)) (cdr alst)))))))
      (loop alst))))

(define depth
  (lambda (alst)
    (cond
      ((null? alst) 1)
      ((not (pair? (car alst))) (depth (cdr alst)))
      ((< (+ (depth (car alst)) 1) (depth (cdr alst)))
       (depth (cdr alst)))
      (else (+ (depth (car alst)) 1)))))

(define depth-with-let
  (lambda (alst)
    (if (null? alst)
        1
        (let ((drest (depth-with-let (cdr alst))))
          (if (pair? (car alst))
              (let ((dfirst (+ (depth-with-let (car alst)) 1)))
                (if (< dfirst drest) drest dfirst))
              drest)))))

;;; Exercise 8.5.4 : page 281

(define andmap
  (lambda (f ls)
    (if (null? ls)
        #t
        (and (f (car ls)) (andmap f (cdr ls))))))

;;; Figure 8.6.1 : page 282

(define cps-simple
  (lambda (exp)
    (variant-case exp
      (proc (formals body)
        (make-proc (append formals '(k)) (cps-exp body)))
      (else (positional-substitution exp
              (map (lambda (head-or-tail-exp)
                     (cons head-or-tail-exp
                       (cps-simple head-or-tail-exp)))
                (append (head-exps exp) (tail-exps exp))))))))

;;; Figure 8.6.2 : page 283

(define cps-exp
  (lambda (exp)
    (if (simple? exp)
        (c-simple exp)
        (let ((init-exp (initial-exp exp)))
          (if (app? init-exp)
              (if (eq? init-exp exp)
                  (c-eta init-exp)
                  (c-app init-exp exp))
              (c-special init-exp exp))))))

;;; Figure 8.6.3 : page 283

(define next-symbol-right
  (lambda (sym)
    (string->symbol 
      (string-append (symbol->string sym) ":"))))

(define next-symbol-left
  (let ((c 0))
    (lambda (rator)
      (set! c (+ c 1))
      (string->symbol
        (string-append ":"
          (if (varref? rator) 
              (symbol->string (varref->var rator))
              "g")
          (number->string c))))))

;;; Figure 8.6.4 : page 284

(define c-simple
  (lambda (exp)
    (make-app (make-varref 'k) (list (cps-simple exp)))))

(define c-app
  (lambda (init-exp exp)
    (make-app (cps-simple (app->rator init-exp))
      (append (map cps-simple (app->rands init-exp))
        (list (let ((g (next-symbol-left (cps-simple (app->rator init-exp)))))
                (make-proc (list g)
                  (cps-exp
                    (positional-substitution exp
                      (list (cons init-exp (make-varref g))))))))))))

(define c-eta
  (lambda (init-exp)
    (make-app (cps-simple (app->rator init-exp))
      (append 
        (map cps-simple (app->rands init-exp))
        (list (make-varref 'k))))))

(define c-special  ; improved for second printing
  (lambda (init-exp exp)
    (let ((new-init-exp (alpha-convert init-exp
                          (intersect
                            (binding-vars init-exp)
                            (free-vars exp)))))
      (positional-substitution new-init-exp
        (append
          (map (lambda (h)
                 (cons h (cps-simple h)))
               (head-exps new-init-exp))
          (map (lambda (t)
                 (cons t (cps-exp
                           (positional-substitution exp
                             (list (cons init-exp t))))))
               (tail-exps new-init-exp)))))))

;;; End of figures for Chapter 8

;;; ****************************************************************

;;; Beginning of figures for Chapter 9

;;; Anonymous figure : page 292, 293, 294, 295, 296

(define remove
  (lambda (s los)
    (remove-cps s los (lambda (v) v))))

(define remove-cps
  (lambda (s los k)
    (cond
      ((null? los) (k '()))
      ((eq? s (car los)) (remove-cps s (cdr los) k))
      (else (remove-cps s (cdr los)
              (lambda (v)
                (k (cons (car los) v))))))))

(define remove
  (lambda (s los)
    (remove-cps s los (make-final-valcont))))

(define remove-cps
  (lambda (s los k)
    (cond
      ((null? los) (apply-continuation k '()))
      ((eq? s (car los)) (remove-cps s (cdr los) k))
      (else (remove-cps s (cdr los) (make-rem1 los k))))))

(define make-final-valcont
  (lambda ()
    (lambda (v) v)))

(define make-rem1
  (lambda (los k)
    (lambda (v)
      (apply-continuation k (cons (car los) v)))))

(define apply-continuation
  (lambda (k v)
    (k v)))

(define-record final-valcont ())
(define-record rem1 (los k))

(define apply-continuation
  (lambda (k v)
    (variant-case k
      (final-valcont () v)
      (rem1 (los k) (apply-continuation k (cons (car los) v))))))

(define subst
  (lambda (new old slst)
    (subst-cps new old slst (lambda (v) v))))

(define subst-cps
  (lambda (new old slst k)
    (if (null? slst)
        (k '())
        (if (not (pair? (car slst)))
            (if (eq? (car slst) old)
                (subst-cps new old (cdr slst)
                  (lambda (v) (k (cons new v))))
                (subst-cps new old (cdr slst)
                  (lambda (v) (k (cons (car slst) v)))))
            (subst-cps new old (car slst)
              (lambda (v)
                (subst-cps new old (cdr slst)
                  (lambda (v1) (k (cons v v1))))))))))

(define make-final-valcont
  (lambda ()
    (lambda (v) v)))

(define make-subst1
  (lambda (new k)
    (lambda (v)
      (apply-continuation k (cons new v)))))

(define make-subst2
  (lambda (slst k)
    (lambda (v)
      (apply-continuation k (cons (car slst) v)))))

(define make-subst3
  (lambda (new old slst k)
    (lambda (v)
      (subst-cps new old (cdr slst)
        (make-subst1 v k)))))

(define apply-continuation
  (lambda (k v)
    (k v)))

(define subst
  (lambda (new old s)
    (subst-cps new old s (make-final-valcont))))

(define subst-cps
  (lambda (new old slst k)
    (if (null? slst)
        (apply-continuation k '())
        (if (not (pair? (car slst)))
            (if (eq? (car slst) old)
                (subst-cps new old (cdr slst)
                  (make-subst1 new k))
                (subst-cps new old (cdr slst)
                  (make-subst2 slst k)))
            (subst-cps new old (car slst)
              (make-subst3 new old slst k))))))

(define-record final-valcont ())
(define-record subst1 (new k))
(define-record subst2 (slst k))
(define-record subst3 (new old slst k))

(define apply-continuation
  (lambda (k v)
    (variant-case k
      (final-valcont () v)
      (subst1 (new k)
        (apply-continuation k (cons new v)))
      (subst2 (slst k)
        (apply-continuation k (cons (car slst) v)))
      (subst3 (new old slst k)
        (subst-cps new old (cdr slst)
          (make-subst1 v k)))
      (else (error "Invalid continuation:" k)))))

;;; Figure 9.2.1 : page 298

(define eval-exp
  (lambda (exp env)
    (variant-case exp
      (lit (datum) datum)
      (varref (var) (apply-env env var))
      (app (rator rands)
        (let ((proc (eval-exp rator env))
              (args (eval-rands rands env)))
          (apply-proc proc args)))
      (if (test-exp then-exp else-exp)
        (if (true-value? (eval-exp test-exp env))
            (eval-exp then-exp env)
            (eval-exp else-exp env)))
      (proc (formals body) (make-closure formals body env))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands env)
    (if (null? rands)
        '()
        (cons (eval-exp (car rands) env)
              (eval-rands (cdr rands) env)))))

(define apply-proc
  (lambda (proc args)
    (variant-case proc
      (prim-proc (prim-op) (apply-prim-op prim-op args))
      (closure (formals body env)
        (eval-exp body (extend-env formals args env)))
      (else (error "Invalid procedure:" proc)))))

;;; Figure 9.2.2 : page 300

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (lit (datum) (k datum))
      (varref (var) (k (apply-env env var)))
      (app (rator rands)
        (eval-exp rator env
          (lambda (proc)
            (eval-rands rands env
              (lambda (all)
                (apply-proc proc all k))))))
      (if (test-exp then-exp else-exp)
        (eval-exp test-exp env
          (lambda (test)
            (if (true-value? test)
                (eval-exp then-exp env k)
                (eval-exp else-exp env k)))))
      (proc (formals body) (k (make-closure formals body env)))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
        (k '())
        (eval-exp (car rands) env
          (lambda (first)
            (eval-rands (cdr rands) env
              (lambda (rest)
                (k (cons first rest)))))))))

(define apply-proc
  (lambda (proc args k)
    (variant-case proc
      (prim-proc (prim-op) (k (apply-prim-op prim-op args)))
      (closure (formals body env)
        (eval-exp body (extend-env formals args env) k))
      (else (error "Invalid procedure:" proc)))))

;;; Figure 9.2.3 : page 301

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (lit (datum) 
        (apply-continuation k datum))
      (varref (var) 
        (apply-continuation k (apply-env env var)))
      (app (rator rands) 
        (eval-exp rator env 
          (make-proc-valcont rands env k)))
      (if (test-exp then-exp else-exp)
        (eval-exp test-exp env
          (make-test-valcont then-exp else-exp env k)))
      (proc (formals body)
        (apply-continuation k 
          (make-closure formals body env)))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
        (apply-continuation k '())
        (eval-exp (car rands) env
          (make-first-valcont rands env k)))))

(define apply-proc
  (lambda (proc args k)
    (variant-case proc
      (prim-proc (prim-op) 
        (apply-continuation k 
          (apply-prim-op prim-op args)))
      (closure (formals body env)
        (eval-exp body (extend-env formals args env) k))
      (else (error "Invalid procedure:" proc)))))

;;; Figure 9.2.4 : page 303

(define make-final-valcont
  (lambda ()
    (lambda (final) final)))

(define make-proc-valcont
  (lambda (rands env k)
    (lambda (proc)
      (eval-rands rands env (make-all-argcont proc k)))))

(define make-all-argcont
  (lambda (proc k)
    (lambda (all)
      (apply-proc proc all k))))

(define make-test-valcont
  (lambda (then-exp else-exp env k)
    (lambda (test)
      (if (true-value? test)
          (eval-exp then-exp env k)
          (eval-exp else-exp env k)))))

(define make-first-valcont
  (lambda (rands env k)    
    (lambda (first)
      (eval-rands (cdr rands) env
        (make-rest-argcont first k)))))

(define make-rest-argcont
  (lambda (first k)
    (lambda (rest)
      (apply-continuation k (cons first rest)))))

(define apply-continuation
  (lambda (k val) 
    (k val)))

;;; Figure 9.2.5 : page 304

(define-record final-valcont ())
(define-record proc-valcont (rands env k))
(define-record all-argcont (proc k))
(define-record test-valcont (then-exp else-exp env k))
(define-record first-valcont (rands env k))
(define-record rest-argcont (first k))

(define apply-continuation
  (lambda (k val)
    (variant-case k     
      (final-valcont ()
        (let ((final val))
          final))
      (proc-valcont (rands env k)
        (let ((proc val))
          (eval-rands rands env 
            (make-all-argcont proc k))))
      (all-argcont (proc k) 
        (let ((all val))
          (apply-proc proc all k)))
      (test-valcont (then-exp else-exp env k)
        (let ((test val))
          (if (true-value? test)
              (eval-exp then-exp env k)
              (eval-exp else-exp env k))))
      (first-valcont (rands env k)
        (let ((first val))
          (eval-rands (cdr rands) env
            (make-rest-argcont first k))))
      (rest-argcont (first k)
        (let ((rest val))
          (apply-continuation k (cons first rest)))))))

;;; Anonymous figure : page 306

(define final-valcont (make-final-valcont))

(define apply-proc
  (lambda (proc args k)
    (variant-case proc
      (prim-proc (prim-op)
        (apply-continuation k (apply-prim-op prim-op args)))
      (closure (formals body env)
        (eval-exp body (extend-env formals args env) k))
      (abort () (apply-continuation final-valcont (car args)))
      (else (error "Invalid procedure:" proc)))))

;;; Anonymous figure : page 307, 308

(define-record continuation (cont))

(define apply-proc
  (lambda (proc args k)
    (variant-case proc
      (prim-proc (prim-op)
        (apply-continuation k (apply-prim-op prim-op args)))
      (closure (formals body env)
        (eval-exp body (extend-env formals args env) k))
      (abort ()
        (apply-continuation final-valcont (car args)))
      (continuation (cont)
        (apply-continuation cont (car args)))
      (else (error "Invalid procedure:" proc)))))

(define-record letcont (var body))

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (letcont (var body)
        (eval-exp body
          (extend-env (list var) (list (make-continuation k)) env)
          k))
      (else ...))))

;;; Anonymous figure : page 310

(define-record callcc-proc ())

(define apply-proc
  (lambda (proc args k)
    (variant-case proc
      (prim-proc (prim-op)
        (apply-continuation k (apply-prim-op prim-op args)))
      (closure (formals body env)
        (eval-exp body (extend-env formals args env) k))
      (continuation (cont) 
        (apply-continuation cont (car args)))
      (callcc-proc () 
        (apply-proc
          (car args)
          (list (make-continuation k))
          k))
      (else (error "Invalid Procedure:" proc)))))

;;; Anonymous figure : page 323

(define-record context-node (prelude postlude creator context-cell))

;;; Figure 9.5.3 : page 324

(define make-initial-context-graph
  (lambda ()
    (make-context-node '* '* #t (make-cell '()))))

(define **current** (make-initial-context-graph))

;;; Figure 9.5.4 : page 325

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (wind (pre body post)
        (create-context
          (wrap-thunk (make-thunk pre env))
          (make-thunk body env)
          (wrap-thunk (make-thunk post env))
          k))
      (letcont (var body)
        (eval-exp body
          (extend-env
            (list var)
            (list (make-cell
                    (make-continuation k **current**)))
            env)
          k))
      (else ...))))

(define create-context
  (lambda (prelude body postlude k)
    (let ((saved-context **current**))
      (set! **current**
        (make-context-node prelude postlude saved-context (make-cell '())))
      (cell-set! (context-node->context-cell saved-context) **current**)
      (apply-thunk prelude
        (lambda (ignored)
          (apply-thunk body
            (lambda (val) (throw k val saved-context))))))))

(define apply-proc
  (lambda (proc args k)
    (variant-case proc
      (prim-proc (prim-op) ...)
      (closure (formals body env) ...)
      (continuation (cont saved-context-node)
        (throw cont (car args) saved-context-node)))))

(define-record continuation (cont saved-context-node))

;;; Figure 9.5.5 : page 326

(define make-thunk
  (lambda (exp env)
    (lambda (k)
      (eval-exp exp env k))))

(define apply-thunk
  (lambda (thunk k)
    (thunk k)))

(define wrap-thunk
  (lambda (thunk)
    (lambda (k)
      (let ((saved-context **current**))
        (set! **current** (make-initial-context-graph))
        (apply-thunk thunk
          (lambda (ignored)
            (set! **current** saved-context)
            (k '*)))))))

;;; Figure 9.5.7 : page 328

(define throw
  (lambda (k value saved-context)
    (if (not (eq? (reverse-context-links! saved-context) **current**))
        (error "Context violation")
        (postlude-walk **current**
          (lambda (top-node)
            (prelude-walk (cell-ref (context-node->context-cell top-node))
              (lambda (ignored)
                (set! **current** saved-context)
                (apply-continuation k value))))))))

(define reverse-context-links!
  (letrec
    ((loop
       (lambda (node last)
         (let ((cell (context-node->context-cell node)))
           (let ((next (cell-ref cell)))
             (cell-set! cell last)
             (if (null? next) node (loop next node)))))))
    (lambda (node) 
      (if (null? node) node (loop node '())))))

(define postlude-walk
  (lambda (node k)
    (if (top-node? node)
        (k node)
        (apply-thunk (context-node->postlude node)
          (lambda (ignored)
            (postlude-walk (cell-ref (context-node->context-cell node)) k))))))
            
(define prelude-walk
  (lambda (node k)
    (if (null? node)
        (k '*)
        (let ((next (cell-ref (context-node->context-cell node))))
          (apply-thunk (context-node->prelude node)
            (lambda (ignored) (prelude-walk next k)))))))

(define top-node?
  (lambda (node)
    (not (eq? (cell-ref (context-node->context-cell node))
              (context-node->creator node)))))

;;; Anonymous figure : page 332

(define make-swap-thunk
  (lambda (cell-1 cell-2)
    (lambda (k)
      (cell-swap! cell-1 cell-2)
      (k '*))))

;;; End of figures for Chapter 9

;;; ****************************************************************

;;; Beginning of figures for Chapter 10

;;; Anonymous figure : page 336, 337, 338

(define member?
  (lambda (s los)
    (cond 
      ((null? los) #f)
      ((equal? (car los) s) #t)
      (else (member? s (cdr los))))))

(define member?
  (lambda (s los)
    (cond 
      ((null? los) #f)
      ((equal? (car los) s) #t)
      (else (set! los (cdr los))
            (member? s los)))))

(define s-reg '*)
(define los-reg '*)

(define member?
  (lambda (s los)
    (set! s-reg s)
    (set! los-reg los)
    (member?/reg)))

(define member?/reg
  (lambda ()
    (cond
      ((null? los-reg) #f)
      ((equal? (car los-reg) s-reg) #t)
      (else (set! los-reg (cdr los-reg))
            (member?/reg)))))

(define n-reg '*)
(define a-reg '*)

(define fact-iter
  (lambda (n)
    (set! n-reg n)
    (set! a-reg 1)
    (fact-iter-acc/reg)))

(define fact-iter-acc/reg
  (lambda ()
    (if (zero? n-reg)
        a-reg
        (begin
          (set! a-reg (* n-reg a-reg))
          (set! n-reg (- n-reg 1))
          (fact-iter-acc/reg)))))

(define fact-iter-acc/reg
  (lambda ()
    (if (zero? n-reg)
        a-reg
        (let ((temp-n-reg n-reg))
          (set! n-reg (- n-reg 1))
          (set! a-reg (* temp-n-reg a-reg))
          (fact-iter-acc/reg)))))

;;; Exercise 10.1.1 : page 338. 339

(define reverse
  (lambda (ls)
    (reverse-acc ls '())))

(define reverse-acc
  (lambda (ls a)
    (if (null? ls)
        a
        (reverse-acc (cdr ls) (cons (car ls) a)))))

(define fib
  (lambda (n)
    (fib-acc n 0 1)))

(define fib-acc
  (lambda (n a1 a2)
    (if (= n 0)
        a2
        (fib-acc (- n 1) a2 (+ a1 a2)))))

;;; Anonymous figure : page 339

(define subst
  (lambda (new old s)
    (subst-cps new old s (lambda (x) x))))

(define subst-cps
  (lambda (new old s k)
    (if (pair? s)
        (subst-cps new old (car s)
          (lambda (v)
            (subst-cps new old (cdr s)
              (lambda (w)
                (k (cons v w))))))
        (if (eq? s old) (k new) (k s)))))

;;; Figure 10.1.1 : page 340

(define-record final-valcont ())
(define-record subst1-cont (s k))
(define-record subst2-cont (v k))

(define new-reg '*)     (define s-reg '*)
(define old-reg '*)     (define k-reg '*)
(define v-reg '*)

(define subst
  (lambda (new old s)
    (set! k-reg (make-final-valcont))
    (set! new-reg new)
    (set! old-reg old)
    (set! s-reg s)
    (subst-cps/reg)))

(define subst-cps/reg
  (lambda ()
    (if (pair? s-reg)
        (begin (set! k-reg (make-subst1-cont s-reg k-reg))
               (set! s-reg (car s-reg))
               (subst-cps/reg))
        (if (eq? s-reg old-reg)
            (begin (set! v-reg new-reg)
                   (apply-continuation/reg))
            (begin (set! v-reg s-reg)
                   (apply-continuation/reg))))))

(define apply-continuation/reg
  (lambda ()
    (variant-case k-reg
      (final-valcont () v-reg)
      (subst1-cont (s k)
        (set! k-reg (make-subst2-cont v-reg k))
        (set! s-reg (cdr s))
        (subst-cps/reg))
      (subst2-cont (v k)
        (set! k-reg k)
        (set! v-reg (cons v v-reg))
        (apply-continuation/reg)))))

;;; Anonymous figure : page 341, 343

(define-record final-valframe ())
(define-record subst1-frame (s))
(define-record subst2-frame (v))

(define make-final-valcont
  (lambda ()
    (cons (make-final-valframe) '())))

(define make-subst1-cont
  (lambda (s k)
    (cons (make-subst1-frame s) k)))

(define make-subst2-cont
  (lambda (v k)
    (cons (make-subst2-frame v) k)))

(define apply-continuation/reg
  (lambda ()
    (let ((frame (car k-reg)) (k (cdr k-reg)))
      (variant-case frame
        (final-valframe () v-reg)
        (subst1-frame (s)
          (set! k-reg (make-subst2-cont v-reg k))
          (set! s-reg (cdr s))
          (subst-cps/reg))
        (subst2-frame (v)
          (set! k-reg k)
          (set! v-reg (cons v v-reg))
          (apply-continuation/reg))))))

;;; Figure 10.2.2 : page 344

(define subst
  (lambda (new old s)
    (push! (make-final-valframe))
    (set! new-reg new)
    (set! old-reg old)
    (set! s-reg s)
    (subst-cps/reg)))

(define subst-cps/reg
  (lambda ()
    (if (pair? s-reg)
        (begin
          (push! (make-subst1-frame s-reg))
          (set! s-reg (car s-reg))
          (subst-cps/reg))
        (if (eq? s-reg old-reg)
            (begin 
               (set! v-reg new-reg)
               (apply-continuation/reg))
            (begin 
               (set! v-reg s-reg)
               (apply-continuation/reg))))))

(define apply-continuation/reg
  (lambda ()
    (let ((frame (top)))
      (pop!)
      (variant-case frame
        (final-valframe () v-reg)
        (subst1-frame (s)
          (push! (make-subst2-frame v-reg))
          (set! s-reg (cdr s))
          (subst-cps/reg))
        (subst2-frame (v)
          (set! v-reg (cons v v-reg))
          (apply-continuation/reg))))))

;;; Figure 10.2.3 : page 345

(define subst
  (lambda (new old s)
    (push! 'final-valframe)
    (set! new-reg new)
    (set! old-reg old)
    (set! s-reg s)
    (subst-cps/reg)))

(define subst-cps/reg
  (lambda ()
    (if (pair? s-reg)
        (begin (push! s-reg)
               (push! 'subst1-frame)
               (set! s-reg (car s-reg))
               (subst-cps/reg))
        (if (eq? s-reg old-reg)
            (begin (set! v-reg new-reg)
                   (apply-continuation/reg))
            (begin (set! v-reg s-reg)
                   (apply-continuation/reg))))))

(define apply-continuation/reg
  (lambda ()
    (let ((frame-tag (top)))
      (pop!)
      (case frame-tag
        ((final-valframe) v-reg)
        ((subst1-frame)
          (let ((s (top)))
            (pop!)
            (push! v-reg)
            (push! 'subst2-frame)
            (set! s-reg (cdr s))
            (subst-cps/reg)))
        ((subst2-frame)
          (let ((v (top)))
            (pop!)
            (set! v-reg (cons v v-reg))
            (apply-continuation/reg)))))))

;;; Figure 10.3.1 : page 348

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (lit (datum) (k datum))
      (varref (var) (apply-env env var k))
      (proc (formals body) (k (make-closure formals body env)))
      (app (rator rands)
        (eval-exp rator env
          (lambda (proc)
            (eval-rands rands env
              (lambda (all)
                (apply-proc proc all k))))))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
        (k '())
        (eval-exp (car rands) env
          (lambda (first)
            (eval-rands (cdr rands) env
              (lambda (rest)
                (k (cons first rest)))))))))

(define apply-proc
  (lambda (proc args k)
    (variant-case proc
      (prim-1 (unary-op)
        (k (unary-op (unary-arg args))))
      (prim-2 (binary-op)
        (k (binary-op (binary-arg-1 args) (binary-arg-2 args))))
      (closure (formals body env)
        (eval-exp body (extend-env formals args env) k))
      (else (error "Invalid primitive operation:" proc)))))

;;; Figure 10.3.2 : page 350

(define eval-exp
  (lambda (exp env vk)
    (variant-case exp
      (lit (datum) (apply-valcont vk datum dummy))
      (varref (var) (apply-env env var vk))
      (proc (formals body) (apply-valcont vk exp env))
      (app (rator rands)
        (eval-exp rator env (make-proc-valcont rands env vk)))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands env ak)
    (if (null? rands)
        (apply-argcont ak '())
        (eval-exp (car rands) env
          (make-first-valcont rands env ak)))))

(define apply-proc
  (lambda (proc proc-env args vk)
    (variant-case proc
      (prim-1 (unary-op)
        (apply-valcont vk
          (unary-op (unary-arg args))
          dummy))
      (prim-2 (binary-op)
        (apply-valcont vk
          (binary-op (binary-arg-1 args) (binary-arg-2 args))
          dummy))
      (proc (formals body)
        (eval-exp body (extend-env formals args proc-env) vk))
      (else (error "Invalid primitive operation:" proc)))))

(define dummy '*)

;;; Figure 10.3.3 : page 351

(define make-final-valcont
  (lambda ()
    (lambda (final final-env)
      (if (eq? final-env dummy)
          final
          '<Procedure>))))

(define make-first-valcont
  (lambda (rands env ak)
    (lambda (first first-env)
      (eval-rands (cdr rands) env
        (make-rest-argcont first first-env ak)))))

(define make-proc-valcont
  (lambda (rands env vk)
    (lambda (proc proc-env)
      (eval-rands rands env
        (make-all-argcont proc proc-env vk)))))

(define apply-valcont
  (lambda (vk val val-env)
    (vk val val-env)))

;;; Anonymous figure : page 351

(define-record rest-argcont (val env-or-dummy ak))
(define-record all-argcont (proc proc-env vk))

(define-record final-valcont ())
(define-record first-valcont (rands env ak))
(define-record proc-valcont (rands env vk))

;;; Figure 10.3.4 : page 352

(define make-rest-argcont ;;; alpha converted
  (lambda (val env-or-dummy ak)
    (lambda (rest)
      (apply-argcont ak
        (cons (cons val env-or-dummy) rest)))))

(define make-all-argcont
  (lambda (proc proc-env vk)
    (lambda (all)
      (apply-proc proc proc-env all vk))))

(define apply-argcont
  (lambda (ak args)
    (ak args)))

;;; Figure 10.3.5 : page 252

(define apply-env
  (lambda (env var vk)
    (env var vk)))

(define extend-env
  (lambda (formals args env)
    (lambda (var vk)
      (if (memq var formals)
          (letrec
            ((loop (lambda (formals rib)
                     (if (eq? var (car formals))
                         (apply-valcont vk
                           (car (car rib))
                           (cdr (car rib)))
                         (loop (cdr formals) (cdr rib))))))
            (loop formals args))
          (apply-env env var vk)))))

;;; Figure 10.3.6 : page 353

(define apply-argcont
  (lambda (ak args)
    (variant-case ak
      (rest-argcont (val env-or-dummy ak)
        (let ((rest args))
          (apply-argcont ak
            (cons (cons val env-or-dummy) rest))))
      (all-argcont (proc proc-env vk)
        (let ((all args))
          (apply-proc proc proc-env all vk))))))

;;; Figure 10.3.7 : page 354

(define eval-rands
  (lambda (rands env ak)
    (if (null? rands)
        (apply-proc ak)
        (eval-exp (car rands) env
          (make-first-valcont rands env ak)))))

(define apply-proc
  (lambda (ak)
    (find-all-argcont ak
      (lambda (proc proc-env vk)
        (variant-case proc
          (prim-1 (unary-op)
            (apply-valcont vk
              (unary-op (unary-arg ak))
              dummy))
          (prim-2 (binary-op)
            (apply-valcont vk
              (binary-op (binary-arg-1 ak) (binary-arg-2 ak))
              dummy))
          (proc (formals body)
            (eval-exp body
              (extend-env formals ak proc-env)
              vk))
          (else (error "Invalid primitive operation:" proc)))))))

(define find-all-argcont
  (lambda (ak receiver)
    (variant-case ak
      (all-argcont (proc proc-env vk)
        (receiver proc proc-env vk))
      (rest-argcont (val env-or-dummy ak)
        (find-all-argcont ak receiver)))))

;;; Anonymous figure : page 355

(define-record initial-env ()) 
(define-record extended-env (formals ak env))

;;; Figure 10.3.8 : page 355

(define extend-env make-extended-env)

(define apply-env
  (lambda (env var vk)
    (variant-case env
      (initial-env () (init-env var vk)) 
      (extended-env (formals ak env)
        (if (memq var formals)
            (letrec
              ((loop (lambda (formals ak)
                       (variant-case ak
                         (rest-argcont (val env-or-dummy ak)
                           (if (eq? var (car formals))
                               (apply-valcont vk val env-or-dummy)
                               (loop (cdr formals) ak)))
                         (else (error "Invalid environment"))))))
              (loop (reverse formals) ak))
            (apply-env env var vk))))))

;;; Figure 10.4.1 : page 357

(define top-ptr '*)
(define the-stack '*)
(define stack-ptr-limit 100)

(define init-stack!
  (lambda ()
    (set! top-ptr -1)
    (set! the-stack (make-vector (+ stack-ptr-limit 1)))))

(define push!
  (lambda (val)
    (if (= top-ptr stack-ptr-limit)
        (error "Stack overflow")
        (begin
          (set! top-ptr (+ 1 top-ptr))
          (vector-set! the-stack top-ptr val)))))

(define pop-to-ptr!
  (lambda (ptr)
    (set! top-ptr ptr)))

(define stack-ref
  (lambda (ptr)
    (vector-ref the-stack ptr)))

(define decrement-ptr
  (lambda (ptr n)
    (- ptr n)))

(define extract
  (lambda (ptr n receiver)
    (letrec
      ((loop (lambda (k)
               (let ((next (decrement-ptr ptr k)))
                 (if (> k n)
                     (list next)
                     (cons (stack-ref next) (loop (+ 1 k))))))))
      (apply receiver (loop 1)))))

;;; Figure 10.4.2 : page 358

(define eval-exp/stack
  (lambda (exp)
    (variant-case exp
      (lit (datum) (apply-valcont/stack datum dummy))
      (varref (var) (apply-env/stack env-reg var))
      (proc (formals body) (apply-valcont/stack exp env-reg))
      (app (rator rands)
        (make-proc-valcont! rands env-reg)
        (eval-exp/stack rator))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands/stack
  (lambda (rands)
    (if (null? rands)
        (apply-proc/stack)
        (begin
          (make-first-valcont! rands env-reg)
          (eval-exp/stack (car rands))))))

;;; Figure 10.4.3 : page 359

(define make-rest-argcont! ;;; alpha converted
  (lambda (val env-or-dummy)
    (push! env-or-dummy)
    (push! val)
    (push! '*rest)))

(define make-all-argcont!
  (lambda (proc proc-env)
    (push! proc-env)
    (push! proc)
    (push! '*all)))

(define make-final-valcont!
  (lambda ()
    (init-stack!)
    (push! '*final)))

(define make-first-valcont!
  (lambda (rands env)
    (push! env)
    (push! rands)
    (push! '*first)))

(define make-proc-valcont!
  (lambda (rands env)
    (push! env)
    (push! rands)
    (push! '*proc)))

;;; Figure 10.4.4 : page 360

(define apply-valcont/stack
  (lambda (val env-or-dummy)
    (case (stack-ref top-ptr)
      ((*final)
       (extract top-ptr 0
         (lambda (vk)
           (pop-to-ptr! vk)
           (if (eq? env-or-dummy dummy) val '<Procedure>))))
      ((*first)
       (extract top-ptr 2
         (lambda (rands env ak)
           (pop-to-ptr! ak)
           (make-rest-argcont! val env-or-dummy)
           (set! env-reg env)
           (eval-rands/stack (cdr rands)))))
      ((*proc)
       (extract top-ptr 2
         (lambda (rands env vk)
           (pop-to-ptr! vk)
           (make-all-argcont! val env-or-dummy)
           (set! env-reg env)
           (eval-rands/stack rands)))))))

;;; Figure 10.4.5 : page 361

(define apply-env/stack
  (lambda (env var)
    (variant-case env
      (initial-env ()
        (apply-valcont/stack (init-env var) dummy))
      (extended-env (formals ak env)
        (if (memq var formals)
            (letrec
              ((loop (lambda (formals ak)
                       (case (stack-ref ak)
                         ((*rest)
                          (extract ak 2
                            (lambda (val env-or-dummy ak)
                              (if (eq? var (car formals))
                                  (apply-valcont/stack val
                                    env-or-dummy)
                                  (loop (cdr formals) ak)))))
                         (else (error "Invalid tag:" ak))))))
              (loop (reverse formals) ak))
            (apply-env/stack env var))))))

;;; Figure 10.4.6 : page 362

(define apply-proc/stack
  (lambda ()
    (find-all-argcont top-ptr
      (lambda (proc proc-env vk)
        (variant-case proc
          (prim-1 (unary-op)
            (let ((ans (unary-op (unary-arg top-ptr))))
              (pop-to-ptr! vk)
              (apply-valcont/stack ans dummy)))
          (prim-2 (binary-op)
            (let ((ans (binary-op
                         (binary-arg-1 top-ptr)
                         (binary-arg-2 top-ptr))))
              (pop-to-ptr! vk)
              (apply-valcont/stack ans dummy)))
          (proc (formals body)
            (let ((ak top-ptr))
              (pop-to-ptr! vk)
              (set! env-reg (extend-env formals ak proc-env))
              (eval-exp/stack body)))
          (else (error "Invalid primitive operation:" proc)))))))

;;; Figure 10.4.7 : page 362

(define find-all-argcont
  (lambda (ptr receiver)
    (case (stack-ref ptr)
      ((*all) (extract ptr 2 receiver))
      ((*rest)
       (extract ptr 2
         (lambda (ignored ignored-env-or-dummy ak)
           (find-all-argcont ak receiver)))))))

;;; Figure 10.4.8 : page 364

(define apply-proc/stack
  (lambda ()
    (find-all-argcont top-ptr
      (lambda (proc proc-env vk)
        (variant-case proc
          (prim-1 (unary-op)
            (let ((ans (unary-op (unary-arg top-ptr))))
              (pop-to-ptr! vk)
              (apply-valcont/stack ans dummy)))
          (prim-2 (binary-op)
            (let ((ans (binary-op
                         (binary-arg-1 top-ptr)
                         (binary-arg-2 top-ptr))))
              (pop-to-ptr! vk)
              (apply-valcont/stack ans dummy)))
          (proc (formals body)
            (let ((ak top-ptr))
              (make-return-valcont! vk)
              (set! env-reg (extend-env formals ak proc-env))
              (eval-exp/stack body)))
          (else (error "Invalid primitive operation:" proc)))))))

(define make-return-valcont!
  (lambda (vk)
    (push! vk)
    (push! '*return)))

;;; Figure 10.4.9 : page 365

(define apply-valcont/stack
  (lambda (val env-or-dummy)
    (case (stack-ref top-ptr)
      ((*final)
       (extract top-ptr 0
         (lambda (vk)
           (pop-to-ptr! vk)
           (if (eq? env-or-dummy dummy) val '<Procedure>))))
      ((*first)
       (extract top-ptr 2
         (lambda (rands env ak)
           (pop-to-ptr! ak)
           (make-rest-argcont! val env-or-dummy)
           (set! env-reg env)
           (eval-rands/stack (cdr rands)))))
      ((*proc)
       (extract top-ptr 2
         (lambda (rands env vk)
           (pop-to-ptr! vk)
           (make-all-argcont! val env-or-dummy)
           (set! env-reg env)
           (eval-rands/stack rands))))
      ((*return)
       (if (eq? env-or-dummy dummy)
           (extract top-ptr 1
             (lambda (vk ak)
               (pop-to-ptr! vk)
               (apply-valcont/stack val env-or-dummy)))
           (error "Can't return procedures"))))))

;;; Anonymous figure : page 365

(define make-return-valcont!
  (lambda (vk env formals)
    (push! formals)
    (push! env)
    (push! vk)
    (push! '*return)))

;;; Figure 10.4.10 : page 367

(define apply-proc/stack
  (lambda ()
    (find-all-argcont top-ptr
      (lambda (proc proc-env vk)
        (variant-case proc
          (prim-1 (unary-op)
            (let ((ans (unary-op (unary-arg top-ptr))))
              (pop-to-ptr! vk)
              (apply-valcont/stack ans dummy)))
          (prim-2 (binary-op)
            (let ((ans (binary-op (binary-arg-1 top-ptr) (binary-arg-2 top-ptr))))
              (pop-to-ptr! vk)
              (apply-valcont/stack ans dummy)))
          (proc (formals body)
            (make-return-valcont! vk proc-env formals)
            (set! env-reg top-ptr)
            (eval-exp/stack body))
          (else (error "Invalid primitive operation:" proc)))))))

(define apply-env/stack
  (lambda (env var)
    (if (= env -1)
        (apply-valcont/stack (init-env var) dummy)
        (case (stack-ref env)
          ((*return)
           (extract env 3
             (lambda (ignored-vk env formals ak)
               (if (memq var formals)
                   (letrec
                     ((loop (lambda (formals ak)
                              (case (stack-ref ak)
                                ((*rest)
                                 (extract ak 2
                                   (lambda (val env-or-dummy ak)
                                     (if (eq? var (car formals))
                                         (apply-valcont/stack val env-or-dummy)
                                         (loop (cdr formals) ak)))))
                                (else (error "Invalid continuation"))))))
                     (loop (reverse formals) ak))
                   (apply-env/stack env var)))))
          (else (error "Invalid environment:" env))))))

;;; End of figures for Chapter 10

;;; ****************************************************************

;;; Beginning of figures for Chapter 11

;;; Figure 11.2.1 : page 380

(define shift
  (lambda (next-action)
    (lambda (buffer char-seq)
      (next-action
        (cons (char-seq-head char-seq) buffer)
        (char-seq-tail char-seq)))))

(define drop
  (lambda (next-action)
    (lambda (buffer char-seq)
      (next-action buffer (char-seq-tail char-seq)))))

(define goto-scanner-state
  (lambda (state)
    (lambda (buffer char-seq)
      (let ((next-action (state (char-seq-head char-seq))))
        (next-action buffer char-seq)))))

(define-record scanner-answer (token unscanned))

(define emit
  (lambda (cooker)
    (lambda (buffer char-seq)
      (make-scanner-answer
        (cooker (reverse buffer))
        char-seq))))

;;; Figure 11.2.2 : page 382

(define-record token (class data))

(define cook-punctuation
  (lambda (class)
    (lambda (buffer)
      (make-token class '*))))

(define cook-number
  (lambda (buffer)
    (make-token 'number
      (string->number (list->string buffer)))))

(define cook-identifier
  (lambda (buffer)
    (let ((symbol (string->symbol (list->string buffer))))
      (if (memq symbol keywords-list)
          (make-token symbol '*)
          (make-token 'variable symbol)))))

;;; Figure 11.2.3 : page 384  (#\nul ==> #\^ throughout the book)

(define scanner-start-state
  (lambda (c)
    (cond
      ((char-whitespace? c)
       (drop (goto-scanner-state scanner-start-state)))      
      ((char-alphabetic? c)
       (shift (goto-scanner-state scanner-identifier-state)))
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-number-state)))
      ((char=? c #\%)
       (drop (goto-scanner-state scanner-comment-state)))
      ((char=? c #\()
       (drop (emit (cook-punctuation 'lparen))))
      ((char=? c #\))
       (drop (emit (cook-punctuation 'rparen))))
      ((char=? c #\^)
       (emit (cook-punctuation 'end-marker)))
      (else (scan-error c)))))

(define scanner-identifier-state
  (lambda (c)
    (cond
      ((char-alphabetic? c)
       (shift (goto-scanner-state scanner-identifier-state)))
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-identifier-state)))
      (else (emit cook-identifier)))))

(define scanner-number-state
  (lambda (c)
    (cond
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-number-state)))
      (else (emit cook-number)))))

(define scanner-comment-state
  (lambda (c)
    (cond
      ((char=? c #\newline)
       (drop (goto-scanner-state scanner-start-state)))
      (else (drop (goto-scanner-state scanner-comment-state))))))

;;; Figure 11.2.4 : page 385

(define scan-once
  (lambda (start-state char-seq)
    ((goto-scanner-state start-state) '() char-seq)))

(define scan-char-seq
  (lambda (start-state char-seq)
    (let ((next-answer (scan-once start-state char-seq)))
      (variant-case next-answer
        (scanner-answer (token unscanned)                 
          (make-token-seq
            token
            (lambda ()
              (if (eq? (token->class token) 'end-marker)
                  '()
                  (scan-char-seq start-state unscanned)))))))))

;;; Anonymous figure : page 387

(define chars-to-scan '*)

(define char-seq-head
  (lambda (n)
    (string-ref chars-to-scan n)))

(define char-seq-tail
  (lambda (n)
    (+ n 1)))

(define scan-string
  (lambda (start-state char-string)
    (set! chars-to-scan
      (string-append char-string (string #\^)))
    (scan-char-seq start-state 0)))

;;; Exercise 11.2.10 : page 388, 389

(define make-empty-buffer
  (lambda ()
    (let ((c (cons '() '*)))
      (set-cdr! c c)
      c)))

(define buffer-contents car)

(define buffer-extend
  (lambda (value buffer)
    (let ((end-cell (cdr buffer))
          (new-cell (cons value '())))
      (set-cdr! end-cell new-cell)
      (if (eq? end-cell buffer)
          (set-car! buffer new-cell)
          (set-cdr! buffer new-cell))
      buffer)))

;;; Anonymous figure : page 391

(define-record compound-command (cmd1 cmd2))
(define-record while-command (exp cmd))
(define-record if-command (exp cmd1 cmd2))
(define-record assignment-command (variable-symbol exp))

(define-record var-expression (variable-symbol))
(define-record constant-expression (number))
(define-record sum-expression (exp1 exp2))

;;; Figure 11.3.1 : page 393

(define check/drop
  (lambda (class next-action)
    (lambda (buffer token-seq)
      (let ((token (token-seq-head token-seq)))
        (if (eq? (token->class token) class)
            (next-action buffer (token-seq-tail token-seq))
            (error "Syntax error: expecting a" class "not a" token))))))

(define check/shift
  (lambda (class next-action)
    (lambda (buffer token-seq)
      (let ((token (token-seq-head token-seq)))
        (if (eq? (token->class token) class)
            (next-action (cons (token->data token) buffer)
              (token-seq-tail token-seq))
            (error "Syntax error: expecting a" class "not a" token))))))

(define-record parser-answer (tree unparsed))

(define reduce
  (lambda (prod-name)
    (lambda (buffer token-seq)
      (make-parser-answer
        (apply (get-constructor-from-name prod-name) (reverse buffer))
        token-seq))))

(define goto-parser-state
  (lambda (state)
    (lambda (buffer token-seq)
      (let ((next-action (state (token-seq-head token-seq))))
        (next-action buffer token-seq)))))

(define process-nt
  (lambda (state next-action)
    (lambda (buffer token-seq)
      (let ((next-answer ((goto-parser-state state) '() token-seq)))
        (next-action (cons (parser-answer->tree next-answer) buffer)
          (parser-answer->unparsed next-answer))))))

(define emit-list
  (lambda ()
    (lambda (buffer token-seq)
      (make-parser-answer (reverse buffer) token-seq))))

;;; Anonymous figure : page 394

(define get-constructor-from-name
  (lambda (prod-name)
    (case prod-name
      ((compound-command) make-compound-command)
      ((while-command) make-while-command)
      ((if-command) make-if-command)
      ((assignment-command) make-assignment-command)
      ((var-expression) make-var-expression)
      ((constant-expression) make-constant-expression)
      ((sum-expression) make-sum-expression)
      (else (error "Invalid production name:" prod-name)))))

;;; Figure 11.3.2 : page 396

(define parse-command
  (lambda (token)
    (case (token->class token)
      ((begin)
       (check/drop 'begin
         (process-nt parse-command
           (check/drop 'semicolon
             (process-nt parse-command
               (check/drop 'end
                 (reduce 'compound-command)))))))
      ((variable)
       (check/shift 'variable
         (check/drop 'assign-sym
           (process-nt parse-expression
             (reduce 'assignment-command)))))
      (else (parse-error token)))))

(define parse-expression
  (lambda (token)
    (case (token->class token)
      ((variable)
       (check/shift 'variable
         (reduce 'var-expression)))
      ((number)
       (check/shift 'number
         (reduce 'constant-expression)))
      ((lparen)
       (check/drop 'lparen
         (process-nt parse-expression
           (check/drop 'plus-sym
             (process-nt parse-expression
               (check/drop 'rparen
                 (reduce 'sum-expression)))))))
      (else (parse-error token)))))

;;; Figure 11.3.3 : page 397
(define parse-once
  (lambda (start-state token-seq)
    ((goto-parser-state start-state) '() token-seq)))

(define parse-token-seq
  (lambda (start-state token-seq)
    (let ((answer (parse-once start-state token-seq)))
      (variant-case answer
        (parser-answer (tree unparsed)
          (if (eq? (token->class (token-seq-head unparsed)) 
                   'end-marker)
              tree
              (error "Tokens left over:" unparsed)))))))

;;; Anonymous figure : page 399

(define-record compound-command (cmd-list))

;;; Figure 11.3.4 : page 400

(define parse-command
  (lambda (token)
    (case (token->class token)
      ((begin)
       (check/drop 'begin
         (process-nt parse-command-list
           (check/drop 'end
             (reduce 'compound-command)))))
      (else ...))))

(define parse-command-list
  (lambda (token)
    (process-nt parse-command
      (goto-parser-state command-list-loop))))

(define command-list-loop
  (lambda (token)
    (case (token->class token)
      ((semicolon)
       (check/drop 'semicolon
         (process-nt parse-command
           (goto-parser-state command-list-loop))))
      ((end) (emit-list)))))

;;; Figure 11.4.1 : page 408

(define parse-command
  (lambda (buffer token-seq)
    (case (token->class (token-seq-head token-seq))
      ((begin)
       (if (eq? (token->class (token-seq-head token-seq)) 'begin)
           (let ((buffer buffer)                  
                 (token-seq (token-seq-tail token-seq)))
             (let ((next-answer (parse-command '() token-seq)))
               (variant-case next-answer
                 (parser-answer (tree unparsed)
                   (let ((buffer (cons tree buffer))
                         (token-seq unparsed))
                     (if (eq? (token->class (token-seq-head token-seq)) 'semicolon)
                         (let ((buffer buffer)
                               (token-seq (token-seq-tail token-seq)))
                           (let ((next-answer (parse-command '() token-seq)))
                             (variant-case next-answer
                               (parser-answer (tree unparsed)
                                 (let ((buffer (cons tree buffer))
                                       (token-seq unparsed))
                                   (if (eq? (token->class (token-seq-head token-seq))
                                            'end)
                                       (let ((buffer buffer)
                                             (token-seq (token-seq-tail token-seq)))
                                         (make-parser-answer
                                           (apply make-compound-command
                                             (reverse buffer))
                                           token-seq))))))))))))))))
      ((variable)
       (if (eq? (token->class (token-seq-head token-seq)) 'variable)
           (let ((buffer (cons (token->data (token-seq-head token-seq)) buffer))
                 (token-seq (token-seq-tail token-seq)))
             (if (eq? (token->class (token-seq-head token-seq)) 'assign-sym)
                 (let ((buffer buffer)
                       (token-seq (token-seq-tail token-seq)))
                   (let ((next-answer (parse-expression '() token-seq)))
                     (variant-case next-answer
                       (parser-answer (tree unparsed)
                         (let ((buffer (cons tree buffer)) (token-seq unparsed))
                           (make-parser-answer
                             (apply make-assignment-command (reverse buffer))
                             token-seq)))))))))))))

;;; Figure 11.4.2 : page 409

(define parse-command
  (lambda (token-seq future-action)
    (case (token->class (token-seq-head token-seq))
      ((begin)
       (parse-command (token-seq-tail token-seq)
         (lambda (tree1 unparsed1)
           (if (eq? (token->class (token-seq-head unparsed1)) 'semicolon)
               (parse-command (token-seq-tail unparsed1)
                 (lambda (tree2 unparsed2)
                   (if (eq? (token->class (token-seq-head unparsed2)) 'end)
                       (future-action
                         (make-compound-command tree1 tree2)
                         (token-seq-tail unparsed2))
                       (parse-error ...))))
               (parse-error ...)))))
      ((variable)
       (let ((unparsed1 (token-seq-tail token-seq)))
         (if (eq? (token->class (token-seq-head unparsed1)) 'assign-sym)
             (parse-expression (token-seq-tail unparsed1)
               (lambda (tree2 unparsed2)
                 (future-action
                   (make-assignment-command
                     (token->data (token-seq-head token-seq))
                     tree2)
                   unparsed2)))
             (parse-error ...))))
      (else (parse-error ...)))))

;;; Figure 11.5.1 : 411, 412

(define check/drop
  (lambda (class next-action)
    (lambda (buffer token-seq grammar)
      (let ((token (token-seq-head token-seq)))
        (if (eq? (token->class token) class)
            (apply-parser-action next-action
              buffer
              (token-seq-tail token-seq)
              grammar)
            (error "Syntax error: expecting a" class "not a" token))))))

(define check/shift
  (lambda (class next-action)
    (lambda (buffer token-seq grammar)
      (let ((token (token-seq-head token-seq)))
        (if (eq? (token->class token) class)
            (apply-parser-action next-action 
              (cons (token->data token) buffer)
              (token-seq-tail token-seq)
              grammar)
            (error "Syntax error: expecting a" class "not a" token))))))

(define reduce 
  (lambda (prod-name)
    (lambda (buffer token-seq grammar)
      (make-parser-answer
        (apply (get-constructor-from-name prod-name)
               (reverse buffer))
        token-seq))))

(define goto-label
  (lambda (label)
    (lambda (token-seq buffer grammar)
      (let ((next-action
              (apply-parser-state
                (apply-parser-grammar grammar label)
                (token-seq-head token-seq))))
        (apply-parser-action next-action
           buffer 
           token-seq
           grammar)))))

(define process-nt
  (lambda (label next-action)
    (lambda (buffer token-seq grammar)
      (let ((next-answer 
              (apply-parser-action
                (goto-label label) '() token-seq grammar)))
        (variant-case next-answer
          (parser-answer (tree unparsed)
            (apply-parser-action next-action
              (cons tree buffer)
              unparsed
              grammar)))))))

(define emit-list
  (lambda ()
    (lambda (buffer token-seq grammar)
      (make-parser-answer (reverse buffer) token-seq))))

;;; Anonymous figure : page 412

(define-record check/shift (class))
(define-record check/drop (class))
(define-record process-nt (label))
(define-record reduce (prod-name))
(define-record goto-label (label))
(define-record emit-list ())

;;; Figure 11.5.2 : page 414

(define apply-parser-action
  (lambda (action buffer token-seq grammar)
    (let ((first-frame (car action))
          (next-action (cdr action)))
      (variant-case first-frame
        (check/drop (class)
          (let ((token (token-seq-head token-seq)))
            (if (eq? (token->class token) class)
                (apply-parser-action next-action
                  buffer (token-seq-tail token-seq) grammar)
                (error "Syntax error: expecting a" class "not a" token))))
        (check/shift (class)
          (let ((token (token-seq-head token-seq)))
            (if (eq? (token->class token) class)
                (apply-parser-action next-action 
                  (cons (token->data token) buffer)
                  (token-seq-tail token-seq)
                  grammar)
                (error "Invalid token class" class token))))
        (process-nt (label)
          (let ((next-answer (apply-parser-action
                               (apply-parser-state (apply-grammar grammar label)
                                 (token-seq-head token-seq))
                               '() token-seq grammar)))
            (variant-case next-answer
              (parser-answer (tree unparsed)
                (apply-parser-action next-action
                  (cons tree buffer) unparsed grammar)))))
        (reduce (prod-name)
          (make-parser-answer
            (apply (get-constructor-from-name prod-name) (reverse buffer))
            token-seq))
        (goto-label (label)
          (let ((next-action (apply-parser-state (apply-grammar grammar label)
                               (token-seq-head token-seq))))
            (apply-parser-action next-action buffer token-seq grammar)))
        (emit-list () (make-parser-answer (reverse buffer) token-seq))
        (else (error "Invalid frame:" first-frame))))))

;;; Figure 11.5.3 : page 415

(define apply-parser-state
  (lambda (state token)
    (letrec
      ((loop
         (lambda (alternatives)
           (if (null? alternatives)
               (error "Invalid token:" token)
               (let ((this-alternative (car alternatives)))
                 (cond
                   ((eq? (car this-alternative) 'else)
                    (cdr this-alternative))
                   ((memq (token->class token)
                          (car this-alternative))
                    (cdr this-alternative))
                   (else (loop (cdr alternatives)))))))))
      (loop state))))

;;; Figure 11.5.4 : page 416

(define parse-once
  (lambda (grammar token-seq)
    (let ((start-state (car (car grammar))))
      (let ((start-action (list (make-goto-label start-state))))
        (apply-parser-action start-action '() token-seq grammar)))))

;;; Anonymous figure : page 416

(define apply-grammar
  (lambda (grammar label)
    (cond
      ((null? grammar) (error "Invalid label:" label))
      ((eq? (caar grammar) label) (cdar grammar))
      (else (apply-grammar (cdr grammar) label)))))

;;; Figure 11.5.5 : page 417
(define command-grammar
  '((command
      ((begin)
        #(check/drop begin)
        #(process-nt command)
        #(check/drop semicolon)
        #(process-nt command)
        #(check/drop end)
        #(reduce compound-command))
      ((variable)
        #(check/shift variable)
        #(check/drop assign-sym)
        #(process-nt expression)
        #(reduce assignment-command)))
    (expression
      ((variable)
        #(check/shift variable)
        #(reduce var-expression))
      ((number)
        #(check/shift number)
        #(reduce constant-expression))
      ((lparen)
        #(check/drop lparen)
        #(process-nt expression)
        #(check/drop plus-sym)
        #(process-nt expression)
        #(check/drop rparen)
        #(reduce sum-expression)))))

;;; Figure 11.5.6 : page 418

(define scanner-table
  '((start-state 
      (((whitespace) #(drop) #(goto-label start-state))
       ((alphabetic) #(shift) #(goto-label identifier-state))
       ((numeric) #(shift) #(goto-label number-state))
       ((#\%) #(drop) #(goto-label comment-state))
       ((#\() #(drop) #(emit lparen))
       ((#\)) #(drop) #(emit rparen))
       ((#\^) #(emit end-marker))
       (else #(scan-error))))
    (identifier-state 
      (((alphabetic) #(shift) #(goto-label identifier-state))
       ((numeric) #(shift) #(goto-label identifier-state))
       (else #(emit identifier))))
    (number-state 
      (((numeric) #(shift) #(goto-label number-state))
       (else #(emit number))))
    (comment-state 
      (((#\newline) #(drop) #(goto-label start-state))
       (else #(shift) #(goto-label comment-state))))))

;;; End of figures for Chapter 11

;;; ****************************************************************

;;; Beginning of figures for Chapter 12

;;; Anonymous figure : page 422

(define run
  (lambda (exp)
    (eval-exp (parse exp) init-env (lambda (val) val))))

;;; Figure 12.1.1 : page 423

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (lit (datum) (k datum))
      (varref (var) (k (apply-env env var)))      
      (if (test-exp then-exp else-exp)
        (eval-exp test-exp env
          (lambda (test)
            (if (true-value? test)
                (eval-exp then-exp env k)
                (eval-exp else-exp env k)))))
      (sum (rands)
        (eval-rands rands env
          (lambda (sum-args)
            (k (add-list sum-args)))))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
        (k '())
        (eval-exp (car rands) env
          (lambda (first)
            (eval-rands (cdr rands) env
              (lambda (rest)
                (k (cons first rest)))))))))

;;; Figure 12.1.2 : page 425

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (lit (datum) (lit-action datum k))
      (varref (var) (varref-action var env k))
      (if (test-exp then-exp else-exp)
        (eval-exp test-exp env
          (build-cont
            (lambda (test)
              (test-action test
                (lambda (k)
                  (eval-exp then-exp env k))
                (lambda (k)
                  (eval-exp else-exp env k))
                k)))))
      (sum (rands)
        (eval-rands rands env
          (build-cont
            (lambda (arg-list)
              (sum-action arg-list k)))))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
        (emptyargs-action k)
        (eval-exp (car rands) env
          (build-cont
            (lambda (first)
              (eval-rands (cdr rands) env
                (build-cont
                  (lambda (rest)
                    (rest-action first rest k))))))))))

;;; Anonymous figure : page 426

(define-record cont (reg code))

;;; Figure 12.1.3 : page 427

(define halt-action
  (lambda (val)
    val))

(define lit-action
  (lambda (datum k)
    (k datum)))

(define varref-action
  (lambda (var env k)
    (k (apply-env env var))))

(define test-action
  (lambda (test then-action-thunk else-action-thunk k)
    (if (true-value? test)
        (then-action-thunk k)
        (else-action-thunk k))))

(define sum-action
  (lambda (sum-args k)
    (k (add-list sum-args))))

(define emptyargs-action
  (lambda (k)
    (k '())))

(define rest-action
  (lambda (first rest k)
    (k (cons first rest))))

(define build-cont
  (lambda (k)
    (lambda (v)
      (k v))))

(define run
  (lambda (exp)
    (eval-exp (parse exp) init-env
      (build-cont
        (lambda (val)
          (halt-action val))))))

;;; Figure 12.1.4 : 428

(define-record halt-instruc (val-reg))
(define halt-action make-halt-instruc)

(define-record lit-instruc (datum k))
(define lit-action make-lit-instruc)

(define-record varref-instruc (var env-reg k))
(define varref-action make-varref-instruc)

(define-record sum-instruc (arg-list-reg k))
(define sum-action make-sum-instruc)

(define-record emptyargs-instruc (k))
(define emptyargs-action make-emptyargs-instruc)
  
(define-record rest-instruc (first-reg rest-reg k))
(define rest-action make-rest-instruc)

(define-record test-instruc (test-reg then-code else-code))
(define test-action
  (lambda (test then-action-thunk else-action-thunk k)
    (make-test-instruc test
      (then-action-thunk k)
      (else-action-thunk k))))

;;; Anonymous figure : page 429

(define build-cont
  (lambda (action-generator)
    (let ((reg (allocate-reg)))
      (make-cont reg 
        (action-generator reg)))))

(define compile
  (lambda (exp)
    (eval-exp (parse exp) 'env-reg
      (build-cont
        (lambda (reg)
          (halt-action reg))))))

;;; Figure 12.1.5 : page 432

(define eval-code
  (lambda (code)
    (variant-case code
      (halt-instruc (val-reg) (get-reg val-reg))
      (lit-instruc (datum k) (apply-cont k datum))
      (varref-instruc (var env-reg k) 
        (apply-cont k (apply-env (get-reg env-reg) var)))
      (sum-instruc (arg-list-reg k)
        (apply-cont k (add-list (get-reg arg-list-reg))))
      (emptyargs-instruc (k) (apply-cont k '()))
      (rest-instruc (first-reg rest-reg k)
        (apply-cont k
          (cons (get-reg first-reg) (get-reg rest-reg))))
      (test-instruc (test-reg then-code else-code)
        (if (true-value? (get-reg test-reg))
            (eval-code then-code)
            (eval-code else-code)))
      (else (error "Invalid code:" code)))))

(define apply-cont
  (lambda (k v)
    (variant-case k
      (cont (reg code) (set-reg! reg v) (eval-code code))
      (else (error "Invalid continuation:" k)))))

(define goto-cont
  (lambda (k)
    (variant-case k
      (cont (reg code) (eval-code code))
      (else (error "Invalid continuation:" k)))))

(define build-cont
  (lambda (action-generator)
    (let ((reg (allocate-reg)))
      (make-cont reg 
        (action-generator reg)))))

(define run
  (lambda (exp)
    (eval-code (compile exp))))

;;; Figure 12.1.6 : page 434

(define reg-file
  (extend-ff 'env-reg (make-cell init-env) (create-empty-ff)))

(define get-reg
  (lambda (sym)
    (cell-ref (apply-ff reg-file sym))))

(define set-reg!
  (lambda (sym val)
    (cell-set! (apply-ff reg-file sym) val)))

(define reg-counter 0)

(define allocate-reg
  (lambda ()
    (let ((reg (string->symbol
                 (string-append 
                   (symbol->string 'r)
                   (number->string reg-counter)))))
      (set! reg-file (extend-ff reg (make-cell '*) reg-file))
      (set! reg-counter (+ reg-counter 1))
      reg)))

;;; Anonymous figure : page 435

(define reg-names '(r0 r1 r2 r3 r4 r5))

;;; Anonymous figure : page 436

(define reg-file
  (extend-ff*
    reg-names
    (map (lambda (x) (make-cell '*)) reg-names)
    (extend-ff 'env-reg (make-cell init-env) (create-empty-ff))))

(define allocate-reg
  (lambda (used-regs)
    (letrec
      ((loop 
         (lambda (avail)
           (cond
             ((null? avail) (error "No more registers"))
             ((memq (car avail) used-regs) (loop (cdr avail)))
             (else (car avail))))))
      (loop reg-names))))

(define-record cont (reg free-regs code))

(define build-cont
  (lambda (free-regs action-generator)
    (let ((reg (allocate-reg free-regs)))
      (make-cont reg free-regs (action-generator reg)))))

;;; Figure 12.2.1 : page 437

(define compile
  (lambda (exp)
    (eval-exp (parse exp) 'env-reg 
      (build-cont '()
        (lambda (final) (halt-action final))))))

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (lit (datum) (lit-action datum k))
      (varref (var) (varref-action var env k))
      (if (test-exp then-exp else-exp)
        (eval-exp test-exp env
          (build-cont (cont->free-regs k)
            (lambda (test)
              (test-action test
                (lambda (k)
                  (eval-exp then-exp env k))
                (lambda (k)
                  (eval-exp else-exp env k))
                k)))))
      (sum (rands)
        (eval-rands rands env
          (build-cont (cont->free-regs k)
            (lambda (arg-list)
              (sum-action arg-list k)))))
      (else (error "Invalid abstract syntax:" exp)))))

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
        (emptyargs-action k)
        (eval-exp (car rands) env
          (build-cont (cont->free-regs k)
            (lambda (first)
              (eval-rands (cdr rands) env
                (build-cont (cons first (cont->free-regs k))
                  (lambda (rest)
                    (rest-action first rest k))))))))))

;;; Figure 12.3.1 : page 441

(define-record stop (reg))
(define halt-action
  (lambda (val)
    (cons (make-stop val) '())))

(define-record move-imm (datum reg))
(define lit-action
  (lambda (datum k)
    (cons (make-move-imm datum (cont->reg k))
          (cont->code k))))

(define-record fetch (var env reg))
(define varref-action
  (lambda (var env k)
    (cons (make-fetch var env (cont->reg k))
          (cont->code k))))

(define-record sum-list (sum-args reg))
(define sum-action
  (lambda (sum-args k)
    (cons (make-sum-list sum-args (cont->reg k))
          (cont->code k))))

(define-record cons-1 (reg))
(define emptyargs-action
  (lambda (k)
    (cons (make-cons-1 (cont->reg k))
          (cont->code k))))

(define-record cons-3 (first rest reg))
(define rest-action
  (lambda (first rest k)
    (cons (make-cons-3 first rest (cont->reg k))
          (cont->code k))))

(define-record brz (test else-instruc))
(define test-action
  (lambda (test then-action-thunk else-action-thunk k)
    (cons (make-brz test (else-action-thunk k))
          (then-action-thunk k))))

;;; Figure 12.3.2 : page 442

(define eval-code
  (lambda (code)
    (let ((ir (car code)) (pc (cdr code)))
      (variant-case ir
        (stop (reg) (get-reg reg))
        (move-imm (datum reg)
          (set-reg! reg datum) 
          (eval-code pc))
        (fetch (var env reg)
          (set-reg! reg (apply-env (get-reg env) var))
          (eval-code pc))
        (sum-list (sum-args reg)
          (set-reg! reg (add-list (get-reg sum-args)))
          (eval-code pc))
        (cons-1 (reg)
          (set-reg! reg '())
          (eval-code pc))
        (cons-3 (first rest reg)
          (set-reg! reg (cons (get-reg first) (get-reg rest)))
          (eval-code pc))
        (brz (test else-instruc)
          (if (zero? (get-reg test))
              (eval-code else-instruc)
              (eval-code pc)))
        (else (error "Invalid code:" code))))))

;;; Figure 12.3.3 : page 444

(define ir '*)
(define pc '*)
(define done '*)
(define result '*)

(define eval-code
  (lambda (code)
    (set! pc code)
    (set! done #f)
    (eval-code/reg)))

(define eval-code/reg
  (lambda ()
    (set! ir (car pc))
    (set! pc (cdr pc))
    (variant-case ir
      (stop (reg)
        (set! done #t)
        (set! result (get-reg reg)))
      (move-imm (datum reg)
        (set-reg! reg datum))
      (fetch (var env reg)
        (set-reg! reg (apply-env (get-reg env) var)))
      (sum-list (sum-args reg)
        (set-reg! reg (add-list (get-reg sum-args))))
      (cons-1 (reg)
        (set-reg! reg '()))
      (cons-3 (first rest reg)
        (set-reg! reg (cons (get-reg first) (get-reg rest))))
      (brz (test else-instruc)
        (if (zero? (get-reg test)) (set! pc else-instruc)))
      (else (error "Invalid instruction:" ir)))
    (if done result (eval-code/reg))))

;;; Figure 12.4.1 : page 447

(define-record closure (formals body env))

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (proc (formals body) 
        (k (make-closure formals body env)))
      (app (rator rands)
        (eval-exp rator env
          (lambda (closure)
            (eval-rands rands env
              (lambda (args)
                (apply-closure closure args k))))))
      (else ...))))

(define apply-closure
  (lambda (closure args k)
    (variant-case closure
      (closure (formals body env)
        (eval-exp body (extend-env formals args env) k)))))

;;; Anonymous figure : page 447

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (proc (formals body)
        (close-action formals body env k))
      (app (rator rands)
        (eval-exp rator env
          (build-cont
            (lambda (closure)
              (eval-rands rands env
                (build-cont
                  (lambda (args)
                    (apply-closure closure args k))))))))
      (else ...))))

(define close-action
  (lambda (formals body env k)
    (k (make-closure formals body env))))

;;; Anonymous figure : page 448

(define-record closure (exec env))

(define build-exec
  (lambda (formals body)
    (lambda (env args k)
      (extend-env-action formals args env
        (lambda (extended-env)
          (eval-exp body extended-env
            (lambda (val)
              (return-action val k))))))))

(define extend-env-action
  (lambda (formals args env k)
    (k (extend-env formals args env))))

(define return-action
  (lambda (v k)
    (k v)))

;;; Anonymous figure : page 449

(define eval-exp
  (lambda (exp env k)
    (variant-case exp
      (proc (formals body)
        (close-action (build-exec formals body) env k))
      (app (rator rands) 
        (eval-exp rator env
          (build-cont (cont->free-regs k)
            (lambda (closure)
              (eval-rands rands env
                (build-cont (cons closure (cont->free-regs k))
                  (lambda (args)
                    (apply-closure-action closure args k))))))))
      (else ...))))

(define close-action
  (lambda (exec env k)
    (k (make-closure exec env))))

(define apply-closure-action
  (lambda (closure args k)
    (variant-case closure
      (closure (exec env) (exec env args k)))))

;;; Figure 12.4.2 : page 450

(define-record extend-env-instruc (formals args-reg env-reg k))
(define extend-env-action make-extend-env-instruc)

(define-record return-instruc (return-reg k))
(define return-action make-return-instruc)

(define-record close-instruc (exec env-reg k))
(define close-action make-close-instruc)

(define-record save-instruc (reg-list k))
(define save-action make-save-instruc)

(define-record restore-instruc (k))
(define restore-action make-restore-instruc)

(define-record apply-closure-instruc (closure-reg args-reg k))
(define apply-closure-action 
  (lambda (closure-reg args-reg k)
    (variant-case k
      (cont (reg free-regs code)
        (save-action
          (append '(save-regs k-reg env-reg) free-regs)
          (make-cont 'ignore-reg '()
            (make-apply-closure-instruc closure-reg args-reg
              (make-cont reg '()
                (restore-action
                  (make-cont 'ignore-reg '() code)))))))
      (else (error "Invalid continuation:" k)))))

;;; Anonymous figure : page 450

(define-record exec (formals body))
(define build-exec make-exec)

;;; Anonymous figure : page 451

(define build-exec
  (lambda (formals body)
    (extend-env-action formals 'args-reg 'env-reg
      (make-cont 'env-reg '()
        (eval-exp body 'env-reg
          (build-cont '()
            (lambda (val)
              (return-action val 'k-reg))))))))

;;; Figure 12.4.3 : page 453

(define eval-code
  (lambda (code)
    (variant-case code
      (close-instruc (exec env-reg k)
        (apply-cont k
          (make-closure exec
            (get-reg env-reg))))
      (extend-env-instruc (formals args-reg env-reg k)
        (apply-cont k
          (extend-env formals (get-reg args-reg) (get-reg env-reg))))
      (save-instruc (reg-list k) 
        (set-reg! 'save-regs 
          (map (lambda (reg)
                 (cons reg (get-reg reg)))
               reg-list))
        (goto-cont k))
      (apply-closure-instruc (closure-reg args-reg k)
        (set-reg! 'k-reg k)
        (set-reg! 'args-reg (get-reg args-reg))
        (set-reg! 'env-reg (closure->env (get-reg closure-reg)))
        (eval-code (closure->exec (get-reg closure-reg))))
      (restore-instruc (k)
        (for-each (lambda (file-entry)
                    (set-reg! (car file-entry) (cdr file-entry)))
                  (get-reg 'save-regs))
        (goto-cont k))
      (return-instruc (return-reg k)
        (apply-cont (get-reg k) (get-reg return-reg)))
      (else ...))))

;;; Anonymous figure : page 454

(define reg-file
  (extend-ff*
    (append '(args-reg k-reg save-regs) reg-names)
    (map (lambda (x) (make-cell '*)) 
         (append '(args-reg k-reg save-regs) reg-names))
    (extend-ff 'env-reg (make-cell init-env) (create-empty-ff))))

;;; Figure 12.4.4 : page 456, 457

(define tr-eval-exp
  (lambda (exp env free-regs k)
    (variant-case exp
      (lit (datum) (tr-lit-action datum free-regs k))
      (varref (var) (tr-varref-action var env free-regs k))
      (proc (formals body)
        (tr-close-action (build-exec formals body) env free-regs k))
      (app (rator rands) 
        (eval-exp rator env
          (build-cont free-regs
            (lambda (closure)
              (eval-rands rands env
                (build-cont (cons closure free-regs)
                  (lambda (args)
                    (tr-apply-closure-action closure args free-regs k))))))))
      (if (test-exp then-exp else-exp)
        (eval-exp test-exp env
          (build-cont free-regs
            (lambda (test)
              (tr-test-action test
                (lambda (free-regs k)
                  (tr-eval-exp then-exp env free-regs k))
                (lambda (free-regs k)
                  (tr-eval-exp else-exp env free-regs k))
                free-regs
                k)))))
      (sum (rands)
        (eval-rands rands env
          (build-cont free-regs
            (lambda (arg-list)
              (tr-sum-action arg-list free-regs k)))))
      (else (error "Invalid abstract syntax: " exp)))))

(define tr-return-to
  (lambda (free-regs k-reg)
    (build-cont (cons k-reg free-regs)
      (lambda (new-var)
        (return-action k-reg new-var)))))

(define tr-lit-action
  (lambda (datum free-regs k-reg)
    (make-lit-instruc datum (tr-return-to free-regs k-reg))))

(define tr-varref-action
  (lambda (var env-reg free-regs k-reg)
    (make-varref-instruc var env-reg
      (tr-return-to free-regs k-reg))))

(define tr-sum-action
  (lambda (arg-list-reg free-regs k-reg)
    (make-sum-instruc arg-list-reg (tr-return-to free-regs k-reg))))

(define tr-close-action
  (lambda (exec env-reg free-regs k-reg)
    (make-close-instruc exec env-reg
      (tr-return-to free-regs k-reg))))

(define-record tr-apply-closure-instruc (closure-reg args-reg k-reg))
(define tr-apply-closure-action 
  (lambda (closure-reg args-reg free-regs k-reg)
    (make-tr-apply-closure-instruc closure-reg args-reg k-reg)))

(define build-exec
  (lambda (formals body)
    (extend-env-action formals 'args-reg 'env-reg
      (make-cont 'env-reg '()
        (tr-eval-exp body 'env-reg '() 'k-reg)))))

(define eval-code
  (lambda (code)
    (variant-case code
      (tr-apply-closure-instruc (closure-reg args-reg k-reg)
        (set-reg! 'k-reg (get-reg k-reg))
        (set-reg! 'args-reg (get-reg args-reg))
        (set-reg! 'env-reg (closure->env (get-reg closure-reg)))
        (eval-code (closure->exec (get-reg closure-reg))))
      (else ...))))

;;; Exercise 12.4.11 : page 458

(define run
  (lambda (exp)
    ((compile-exp #t (parse exp)
       initial-symbol-table restore-code)
     '*
     '()
     initial-lex-env
     (lambda (x) x))))

;;; Figure 12.4.5 : page 459

(define lit-action
  (lambda (datum code)
    (lambda (acc val* lex-env k)
      (code datum val* lex-env k))))

(define varref-action
  (lambda (lex-addr code)
    (lambda (acc val* lex-env k)
      (code (lookup-lex-env lex-env lex-addr) val* lex-env k))))

(define if-action
  (lambda (then-code else-code)
    (lambda (acc val* lex-env k)
      (if (true-value? acc)
          (then-code acc val* lex-env k)
          (else-code acc val* lex-env k)))))

(define proc-action
  (lambda (code-of-body code)
    (lambda (acc val* lex-env k)
      (code
        (lambda (args new-k)
          (code-of-body '* '() (extend-lex-env args lex-env) new-k))
        val* lex-env k))))

(define restore-code (lambda (acc val* lex-env k) (k acc)))
                                          
(define invoke-code (lambda (acc val* lex-env k) (acc val* k)))

(define save-action
  (lambda (code1 code2) 
    (lambda (acc val* lex-env k)
      (code1 '* '() lex-env
        (lambda (v) (code2 v val* lex-env k))))))

(define push-action
  (lambda (code)
    (lambda (acc val* lex-env k)
      (code acc (cons acc val*) lex-env k))))

;;; Figure 12.4.6 : page 460

(define compile-exp
  (lambda (tail-pos? exp symbol-table next-code)
    (variant-case exp
      (lit (datum) (lit-action datum next-code))
      (varref (var)
        (varref-action 
          (lexical-addr symbol-table var)
           next-code))
      (if (test-exp then-exp else-exp)
        (compile-exp #f test-exp symbol-table
          (if-action
            (compile-exp tail-pos? then-exp 
              symbol-table 
              next-code)
            (compile-exp tail-pos? else-exp 
              symbol-table 
              next-code))))
      (proc (formals body)
        (proc-action
          (compile-exp #t body
            (extend-symbol-table formals symbol-table)
            restore-code)
          next-code))
      (app (rator rands)
        (let ((app-code (compile-rands rands 
                          symbol-table
                          (compile-exp #f rator symbol-table invoke-code))))
          (if tail-pos? 
              app-code
              (save-action app-code next-code)))))))

(define compile-rands
  (lambda (rands symbol-table next-code)
    (if (null? rands)
        next-code
        (compile-exp #f (car rands) symbol-table
          (push-action 
            (compile-rands (cdr rands) symbol-table next-code))))))

;;; End of figure for Chapter 12

;;; ****************************************************************

;;; Beginning of figures for Appendix B

;;; Figure B.1 : page 465

(define-record define (var exp))
(define-record varref (var))
(define-record lit (datum))
(define-record app (rator rands))
(define-record if (test-exp then-exp else-exp))
(define-record let (decls body))
(define-record decl (var exp))
(define-record proc (formals body))
(define-record varassign (var exp))
(define-record letmutable (decls body))
(define-record begin (exp1 exp2))
(define-record letrecproc (procdecls body))
(define-record procdecl (var formals body))
(define-record letrec (decls body))
(define-record dynassign (var exp body))
(define-record letdynamic (decls body))

;;; Figure B.2 : page 466

(define-record definearray (var dim-exp)) 
(define-record letarray (arraydecls body))
(define-record arrayref (array index))
(define-record arrayassign (array index exp))
(define-record letproc (procdecls body))
(define-record local (decls body))
(define-record keydecl (var exp))

;;; Figure B.2 : page 466 %%%%% This is for 3rd printing %%%%%%

(define-record definearray (var lex-exp))
(define-record letarray (arraydecls body))
(define-record arrayref (array index))
(define-record arrayassign (array index exp))
(define-record letproc (procdecls body))
(define-record local (decls body))
(define-record keydecl (var exp))

;;; Figure B.3 : page 466

(define-record super-meth-app (name rands))
(define-record meth-app (name rands))
(define-record i-varref (var))
(define-record c-varref (var))
(define-record i-varassign (var exp))
(define-record c-varassign (var exp))
(define-record method (formals body))
(define-record new-simpleinst (class-exp)) ; corrected for second printing
(define-record new-simpleclass (c-vars i-vars methdecls init-exp))
(define-record new-class (parent-exp c-vars i-vars methdecls init-exp))
(define-record new-instance (class-exp parent-exp i-vars methdecls))

;;; Figure B.4 : page 466

(define-record abort (exp))
(define-record letcont (var body))
(define-record callcc (exp))
(define-record coroutine (exp))
(define-record wind (pre body post))

;;; Figure B.5 : page 466

(define-record sum (rands))

;;; End of figures for Appendix B

;;; ****************************************************************

;;; Beginning of figures for Appendix D

;;; Anonymous code : page 471

(define error
  (lambda args
    (for-each (lambda (x) (display x) (display " ")) args)
    (newline)
    (reset)))

;;; Figure D.1 : page 472, 473, 474

(define check/drop
  (lambda (class next-action)
    (lambda (buffer token-seq)
      (let ((token (token-seq-head token-seq)))
        (if (eq? (token->class token) class)
            (next-action buffer (token-seq-tail token-seq))
            (error "Syntax error: expecting a" class "not a" token))))))

(define check/shift
  (lambda (class next-action)
    (lambda (buffer token-seq)
      (let ((token (token-seq-head token-seq)))
        (if (eq? (token->class token) class)
            (next-action (cons (token->data token) buffer)
                           (token-seq-tail token-seq))
            (error "Syntax error: expecting a" class "not a" token))))))

(define goto-parser-state
  (lambda (state)
    (lambda (buffer token-seq)
      (let ((next-action (state (token-seq-head token-seq))))
        (next-action buffer token-seq)))))

(define reduce
  (lambda (prod-name)
    (lambda (buffer token-seq)
      (make-parser-answer
        (apply (get-constructor-from-name prod-name) (reverse buffer))
        token-seq))))

(define process-nt
  (lambda (state next-action)
    (lambda (buffer token-seq)
      (let ((next-answer ((goto-parser-state state) '() token-seq)))
        (next-action 
          (cons (parser-answer->tree next-answer) buffer)
          (parser-answer->unparsed next-answer))))))

(define emit-list
  (lambda ()
    (lambda (buffer token-seq)
      (make-parser-answer (reverse buffer) token-seq))))

(define-record parser-answer (tree unparsed))
(define token-seq-head car)
(define token-seq-tail cdr)

(define character-string-parser
  (lambda (a-string)
    (parse-token-seq parse-form
      (character-string-scanner a-string))))

(define parse-token-seq
  (lambda (start-state token-seq)
    (let ((answer (parse-once start-state token-seq)))
      (variant-case answer
        (parser-answer (tree unparsed)
          (if (eq? (token->class (token-seq-head unparsed)) 'end-marker)
              tree
              (error "Tokens left over:" unparsed)))))))

(define parse-once
  (lambda (start-state token-seq)
    ((goto-parser-state start-state) '() token-seq)))

(define parse-loop*
  (lambda (terminator separator action)
    (letrec
      ((more (lambda (token)
               (let ((class (token->class token)))
                 (cond
                   ((eq? class terminator) (emit-list))
                   ((eq? separator class)
                    (check/drop separator
                      (action (goto-parser-state more))))
                   (else (error "Invalid separator token:" token)))))))
      (lambda (token)
        (let ((class (token->class token)))
          (if (eq? class terminator)
              (emit-list)
              (action (goto-parser-state more))))))))

(define parse-loop+
  (lambda (terminator separator action)
    (letrec
      ((more (lambda (token)
               (let ((class (token->class token)))
                 (cond
                   ((eq? class terminator) (emit-list))
                   ((eq? separator class)
                    (check/drop separator
                      (action (goto-parser-state more))))
                   (else (error "Invalid separator token:" token)))))))
      (lambda (token)
        (action (goto-parser-state more))))))

;;; Figure D.2 : page 474, 475

(define parse-form
  (lambda (token)
    ((case (token->class token)
       ((define) seen-define)
       ((definearray) seen-definearray)
       (else parse-exp))
     token)))

(define parse-exp
  (lambda (token)
    ((case (token->class token)
      ((number) seen-number)
      ((variable) seen-variable)
      ((lparen) seen-lparen)
      ((if) seen-if)
      ((let) seen-let)
      ((proc) seen-proc)
      ((begin) seen-begin)
      ((letmutable) seen-letmutable)
      ((letrecproc) seen-letrecproc)
      ((letrec) seen-letrec)
      ((letdynamic) seen-letdynamic)
      ((letarray) seen-letarray)
      ((letproc) seen-letproc)
      ((local) seen-local)
      ((dollar-sign) seen-dollar-sign)
      ((ampersand) seen-ampersand)
      ((double-ampersand) seen-double-ampersand)
      ((method) seen-method)
      ((simpleclass) seen-simpleclass)      
      ((simpleinstance) seen-simpleinstance)
      ((class) seen-class) 
      ((instance) seen-instance)
      ((abort) seen-abort)
      ((letcont) seen-letcont)
      ((callcc) seen-callcc)
      ((coroutine) seen-coroutine)
      ((wind) seen-wind)
      ((sum) seen-sum)
      (else (error "Invalid parse-exp token:" token)))
     token)))

(define parse/var
  (lambda (token)
    (case (token->class token)
      ((lparen) (seen-var&lparen token))
      ((assign-sym) (seen-var&assign-sym token))
      ((lbracket) (seen-var&lbracket token))
      (else (reduce 'varref)))))

;;; Figure D.3 : page 476, 477

(define get-constructor-from-name
  (lambda (prod-name)
    (case prod-name
      ((chain) (lambda (x) x))
      ((lit) make-lit)      
      ((app) make-app)
      ((app/var) (lambda (var rands) (make-app (make-varref var) rands)))
      ((begin) (letrec ((loop (lambda (exps)
                                (if (null? (cdr exps))
                                    (car exps)
                                    (make-begin (car exps) (loop (cdr exps)))))))
                 loop))
      ((define) make-define)
      ((varref) make-varref)
      ((if) make-if)
      ((proc) make-proc)
      ((let) make-let)
      ((letmutable) make-letmutable)
      ((letrecproc) make-letrecproc)
      ((letrec) make-letrec)
      ((decl) make-decl)
      ((procdecl) make-procdecl)
      ((varassign) make-varassign)
      ((dynassign) make-dynassign)
      ((letdynamic) make-letdynamic)
      ((definearray) make-definearray)      
      ((arrayref) make-arrayref)      
      ((arrayref/var) (lambda (var index)
                        (make-arrayref (make-varref var) index)))
      ((arrayassign) make-arrayassign)
      ((arrayassign/var) (lambda (var index val)
                           (make-arrayassign (make-varref var) index val)))
      ((letarray) make-letarray)
      ((letproc) make-letproc)
      ((local) make-local)
      ((stuff-and-keydecls) append)      
      ((keydecl) make-keydecl)
      ((i-var) make-i-varref)
      ((c-var) make-c-varref)
      ((method) make-method)
      ((letcont) make-letcont)
      ((i-varassign) make-i-varassign)
      ((c-varassign) make-c-varassign)
      ((new-simpleinst) make-new-simpleinst)
      ((new-simpleclass) make-new-simpleclass)
      ((new-class) make-new-class)
      ((instance) make-new-instance)
      ((meth-app) (lambda (var rands)
                    (if (null? rands)
                        (error "Invalid method rands:" rands)
                        (let ((rand (car rands)))
                          (if (and (varref? rand)
                                   (eq? (varref->var rand) 'super))
                              (make-super-meth-app var (cdr rands))
                              (make-meth-app var rands))))))
      ((abort) make-abort)
      ((letcont) make-letcont)
      ((callcc) make-callcc)
      ((coroutine) make-coroutine)
      ((wind) make-wind)
      ((sum) make-sum)
      (else (error "Bad production name:" prod-name)))))

;;; Figure D.4 : page 478, 479, 480, 481

(define seen-define
  (lambda (token)
    (check/drop 'define
      (check/shift 'variable
        (check/drop 'eqsign
          (process-nt parse-exp (reduce 'define)))))))

(define seen-number
  (lambda (token)
    (check/shift 'number (reduce 'lit))))

(define seen-variable
  (lambda (token)
    (check/shift 'variable (goto-parser-state parse/var))))

(define seen-lparen
  (lambda (token)
    (check/drop 'lparen
      (process-nt parse-exp
        (check/drop 'rparen
          (goto-parser-state parse-proc))))))

(define seen-if
  (lambda (token)
    (check/drop 'if
      (process-nt parse-exp
        (check/drop 'then
          (process-nt parse-exp
            (check/drop 'else
              (process-nt parse-exp (reduce 'if)))))))))

(define seen-proc
  (lambda (token)
    (check/drop 'proc
      (check/drop 'lparen
        (process-nt parse-varlist
          (check/drop 'rparen
            (process-nt parse-exp (reduce 'proc))))))))

(define seen-begin
  (lambda (token)
    (check/drop 'begin
      (process-nt parse-compound
        (check/drop 'end (reduce 'begin))))))

(define seen-let
  (lambda (token)
    (check/drop 'let
      (process-nt parse-decls
        (check/drop 'in (process-nt parse-exp (reduce 'let)))))))

(define seen-letmutable
  (lambda (token)
    (check/drop 'letmutable
      (process-nt parse-decls
        (check/drop 'in
          (process-nt parse-exp (reduce 'letmutable)))))))

(define seen-letrec
  (lambda (token)
    (check/drop 'letrec
      (process-nt parse-decls
        (check/drop 'in (process-nt parse-exp (reduce 'letrec)))))))

(define seen-letrecproc
  (lambda (token)
    (check/drop 'letrecproc
      (process-nt parse-procdecls
        (check/drop 'in (process-nt parse-exp (reduce 'letrecproc)))))))

(define seen-letdynamic
  (lambda (token)
    (check/drop 'letdynamic
      (process-nt parse-decls
        (check/drop 'in (process-nt parse-exp (reduce 'letdynamic)))))))

(define seen-var&lparen
  (lambda (token)
    (check/drop 'lparen
      (process-nt parse-rands
        (check/drop 'rparen (reduce 'app/var))))))

(define seen-var&assign-sym
  (lambda (token)
    (check/drop 'assign-sym
      (process-nt parse-exp
        (goto-parser-state parse-assign-or-dynassign)))))

(define parse-proc
  (lambda (token)
    (case (token->class token)
      ((lparen)
       (check/drop 'lparen
         (process-nt parse-rands (check/drop 'rparen (reduce 'app)))))
      (else (reduce 'chain)))))

(define parse-assign-or-dynassign
  (lambda (token)
    (case (token->class token)
      ((during)
       (check/drop 'during (process-nt parse-exp (reduce 'dynassign))))
      (else (reduce 'varassign)))))

(define parse-varlist
  (parse-loop* 'rparen 'comma
    (lambda (action) (check/shift 'variable action))))

(define parse-compound   ; corrected for second printing
  (parse-loop* 'end 'semicolon
    (lambda (action) (process-nt parse-exp action))))

(define parse-rands
  (parse-loop* 'rparen 'comma
    (lambda (action)
      (process-nt parse-exp action))))

(define parse-decls
  (parse-loop+ 'in 'semicolon
    (lambda (action) (process-nt parse-decl action))))

(define parse-procdecls
  (parse-loop+ 'in 'semicolon
    (lambda (action) (process-nt parse-procdecl action))))

(define parse-decl
  (lambda (token)
    (case (token->class token)
      ((variable)
       (check/shift 'variable
         (check/drop 'eqsign
           (process-nt parse-exp (reduce 'decl)))))
      (else (error "Invalid parse-decl token:" token)))))

(define parse-procdecl
  (lambda (token)
    (case (token->class token)
      ((variable)
       (check/shift 'variable
         (check/drop 'lparen
           (process-nt parse-varlist
             (check/drop 'rparen
               (check/drop 'eqsign
                 (process-nt parse-exp (reduce 'procdecl))))))))
      (else (error "Invalid parse-procdecl token:" token)))))

;;; Figure D.5 : page 482, 483

(define parse-proc
  (lambda (token)
    (case (token->class token)
      ((lparen)
       (check/drop 'lparen
         (process-nt parse-rands (check/drop 'rparen (reduce 'app)))))
      ((lbracket)
       (check/drop 'lbracket
         (process-nt parse-exp
           (check/drop 'rbracket
             (goto-parser-state parse-assign-or-dynassign)))))
      (else (reduce 'chain)))))

(define seen-var&lbracket
  (lambda (token)
    (check/drop 'lbracket
      (process-nt parse-exp
        (check/drop 'rbracket
          (goto-parser-state
            parse-arrayassign-or-arrayref/var))))))

(define seen-definearray
  (lambda (token)
    (check/drop 'definearray
      (check/shift 'variable
        (check/drop 'lbracket
          (process-nt parse-exp
            (check/drop 'rbracket (reduce 'definearray))))))))

(define seen-letarray
  (lambda (token)
    (check/drop 'letarray
      (process-nt parse-arraydecls
        (check/drop 'in (process-nt parse-exp (reduce 'letarray)))))))

(define seen-letproc
  (lambda (token)
    (check/drop 'letproc
      (process-nt parse-procdecls
        (check/drop 'in (process-nt parse-exp (reduce 'letproc)))))))

(define seen-local
  (lambda (token)
    (check/drop 'local
      (process-nt parse-decls
        (check/drop 'in (process-nt parse-exp (reduce 'local)))))))

(define parse-arrayassign-or-arrayref
  (lambda (token)
    (case (token->class token)
      ((assign-sym)
       (check/drop 'assign-sym
         (process-nt parse-exp (reduce 'arrayassign))))
      (else (reduce 'arrayref)))))

(define parse-arrayassign-or-arrayref/var
  (lambda (token)
    (case (token->class token)
      ((assign-sym)
       (check/drop 'assign-sym
         (process-nt parse-exp (reduce 'arrayassign/var))))
      (else (reduce 'arrayref/var)))))

(define parse-arraydecls
  (parse-loop+ 'in 'semicolon
    (lambda (action) (process-nt parse-arraydecl action))))

(define parse-arraydecl
  (lambda (token)
    (case (token->class token)
      ((variable)
       (check/shift 'variable
         (check/drop 'lbracket
           (process-nt parse-exp
             (check/drop 'rbracket (reduce 'decl))))))
      (else (error "Invalid parse-arraydecl token:" token)))))

;;; Figure D.6 : page 483, 484, 485

(define parse-vars-until-optdecls
  (parse-loop* 'rparen 'comma
    (lambda (action)
      (goto-parser-state
        (lambda (token)
          (case (token->class token)
            ((colon) (emit-list))
            ((variable) (check/shift 'variable action))
            (else (error "Invalid parse-vars-until-optdecls:" token))))))))

(define parse-optdecls
  (parse-loop* 'rparen 'comma
    (lambda (action) (process-nt parse-optdecl action))))

(define parse-optdecl-var
  (lambda (token)
    (case (token->class token)
      ((variable)
       (check/shift 'variable
         (check/drop 'eqsign
           (process-nt parse-exp (reduce 'keydecl)))))
      (else (error "Invalid parse-optdecl-var token:" token)))))

(define parse-optdecl
  (lambda (token)
    (case (token->class token)
      ((colon) (check/drop 'colon
                 (goto-parser-state parse-optdecl-var)))
      (else (error "Invalid parse-optdecl token:" token)))))

(define parse-exps-until-keydecls
  (parse-loop* 'rparen 'comma
    (lambda (action)
      (goto-parser-state
        (lambda (token)
          (case (token->class token)
            ((colon) (emit-list))
            (else (process-nt parse-exp action))))))))

(define parse-keydecls
  (parse-loop* 'rparen 'comma
    (lambda (action) (process-nt parse-keydecl action))))

(define parse-keydecl
  (lambda (token)
    (case (token->class token)
      ((colon)
       (check/drop 'colon
         (check/shift 'variable
           (check/drop 'eqsign
             (process-nt parse-exp (reduce 'keydecl))))))
      (else (error "Invalid parse-opts token:" token)))))

(define parse-varlist
  (lambda (token)
    (process-nt parse-vars-until-optdecls
      (process-nt parse-optdecls (reduce 'stuff-and-keydecls)))))

(define parse-rands
  (lambda (token)
    (process-nt parse-exps-until-keydecls
      (process-nt parse-keydecls (reduce 'stuff-and-keydecls)))))

;;; Figure D.7 : page 485, 486, 487

(define seen-dollar-sign
  (lambda (token)
    (check/drop 'dollar-sign
      (check/shift 'variable
        (check/drop 'lparen
          (process-nt parse-rands
            (check/drop 'rparen (reduce 'meth-app))))))))

(define seen-ampersand
  (lambda (token)
    (check/drop 'ampersand
      (check/shift 'variable (goto-parser-state parse/ivar)))))

(define seen-double-ampersand
  (lambda (token)
    (check/drop 'double-ampersand
      (check/shift 'variable (goto-parser-state parse/cvar)))))

(define seen-method
  (lambda (token)
    (check/drop 'method
      (check/drop 'lparen
        (process-nt parse-varlist
          (check/drop 'rparen
            (process-nt parse-exp (reduce 'method))))))))

(define seen-simpleinstance
  (lambda (token)
    (check/drop 'simpleinstance
      (process-nt parse-exp (reduce 'new-simpleinst)))))

(define seen-simpleclass
  (lambda (token)
    (check/drop 'simpleclass
      (check/drop 'lparen
        (process-nt parse-varlist
          (check/drop 'rparen
            (check/drop 'lparen
              (process-nt parse-varlist
                (check/drop 'rparen
                  (check/drop 'lparen
                    (process-nt parse-methdecls
                      (check/drop 'rparen
                        (process-nt parse-exp
                          (reduce 'new-simpleclass))))))))))))))

(define seen-class
  (lambda (token)
    (check/drop 'class
      (process-nt parse-exp
        (check/drop 'comma
          (check/drop 'lparen
            (process-nt parse-varlist
              (check/drop 'rparen
                (check/drop 'lparen
                  (process-nt parse-varlist
                    (check/drop 'rparen
                      (check/drop 'lparen
                        (process-nt parse-methdecls
                          (check/drop 'rparen
                            (process-nt parse-exp
                              (reduce 'new-class))))))))))))))))

(define seen-instance
  (lambda (token)
    (check/drop 'instance
      (process-nt parse-exp
        (check/drop 'comma
          (process-nt parse-exp
            (check/drop 'comma
              (check/drop 'lparen
                (process-nt parse-varlist
                  (check/drop 'rparen
                    (check/drop 'lparen
                      (process-nt parse-methdecls
                        (check/drop 'rparen (reduce 'instance))))))))))))))

(define parse/ivar
  (lambda (token)
    (case (token->class token)
      ((assign-sym)
       (check/drop 'assign-sym
         (process-nt parse-exp (reduce 'i-varassign))))
      (else (reduce 'i-var)))))

(define parse/cvar
  (lambda (token)
    (case (token->class token)
      ((assign-sym)
       (check/drop 'assign-sym
         (process-nt parse-exp (reduce 'c-varassign))))
      (else (reduce 'c-var)))))

(define parse-methdecls
  (parse-loop* 'rparen 'semicolon
    (lambda (action) (process-nt parse-decl action))))

;;; Figure D.8 : page 488

(define seen-abort
  (lambda (token)
    (check/drop 'abort
      (process-nt parse-exp (reduce 'abort)))))

(define seen-letcont
  (lambda (token)
    (check/drop 'letcont
      (check/shift 'variable
        (check/drop 'in (process-nt parse-exp (reduce 'letcont)))))))

(define seen-callcc
  (lambda (token)
    (check/drop 'callcc
      (process-nt parse-exp (reduce 'callcc)))))

(define seen-coroutine
  (lambda (token)
    (check/drop 'coroutine
      (process-nt parse-exp (reduce 'coroutine)))))

(define seen-wind
  (lambda (token)
    (check/drop 'wind
      (process-nt parse-exp
        (check/drop 'within
          (process-nt parse-exp
            (check/drop 'unwind 
              (process-nt parse-exp (reduce 'wind)))))))))

;;; Figure D.9 : page 488

(define seen-sum
  (lambda (token)
    (check/drop 'sum
      (check/drop 'lparen
        (process-nt parse-rands (check/drop 'rparen (reduce 'sum)))))))

;;; End of figures for Appendix D

;;; ****************************************************************

;;; Beginning of figures for Appendix E

;;; Figure E.1 : page 489, 490

(define shift
  (lambda (next-action)
    (lambda (buffer char-seq)
      (next-action (cons (char-seq-head char-seq) buffer)
        (char-seq-tail char-seq)))))

(define drop
  (lambda (next-action)
    (lambda (buffer char-seq)
      (next-action buffer (char-seq-tail char-seq)))))

(define goto-scanner-state
  (lambda (state)
    (lambda (buffer char-seq)
      ((state (char-seq-head char-seq)) buffer char-seq))))

(define emit
  (lambda (cooker)
    (lambda (buffer char-seq)
      (make-scanner-answer (cooker (reverse buffer)) char-seq))))

(define-record token (class data))

(define-record scanner-answer (token unscanned))

(define char-seq-head car)

(define char-seq-tail cdr)

(define cook-punctuation
  (lambda (class)
    (lambda (buffer) (make-token class '*))))

(define cook-number
  (lambda (buffer)
    (make-token 'number (string->number (list->string buffer)))))

(define cook-identifier
  (lambda (buffer)
    (let ((symbol (string->symbol (list->string buffer))))
      (if (memq symbol keywords-list)
          (make-token symbol '*)
          (make-token 'variable symbol)))))

(define scan-once
  (lambda (start-state char-seq)
    ((goto-scanner-state start-state) '() char-seq)))

(define scan-char-seq
  (lambda (start-state char-seq)
    (let ((next-answer (scan-once start-state char-seq)))
      (variant-case next-answer
        (scanner-answer (token unscanned)                 
          (make-token-seq token
            (lambda ()
              (if (eq? (token->class token) 'end-marker)
                  '()
                  (scan-char-seq start-state unscanned)))))))))

(define character-string-scanner
  (lambda (a-string)
    (scan-char-seq scanner-start-state
      (string->list (string-append a-string (string #\^))))))

(define make-token-seq
  (lambda (token thunk)
    (cons token (thunk))))

;;; Figure E.2 : page 491, 492

(define scanner-start-state
  (lambda (c)
    (cond
      ((char-whitespace? c)
       (drop (goto-scanner-state scanner-start-state)))      
      ((char-alphabetic? c)
       (shift (goto-scanner-state scanner-identifier-state)))
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-number-state)))
      ((char=? c #\%)
       (drop (goto-scanner-state scanner-comment-state)))
      ((char=? c #\()
       (drop (emit (cook-punctuation 'lparen))))
      ((char=? c #\))
       (drop (emit (cook-punctuation 'rparen))))
      ((char=? c #\[)
       (drop (emit (cook-punctuation 'lbracket))))
      ((char=? c #\])
       (drop (emit (cook-punctuation 'rbracket))))
      ((char=? c #\,)
       (drop (emit (cook-punctuation 'comma))))
      ((char=? c #\=)                                                  
       (drop (emit (cook-punctuation 'eqsign))))                       
      ((char=? c #\;)                                                  
       (drop (emit (cook-punctuation 'semicolon))))
      ((char=? c #\&)
       (drop (goto-scanner-state scanner-ampersand-state)))
      ((char=? c #\$)
       (drop (emit (cook-punctuation 'dollar-sign))))
      ((char=? c #\:)
       (drop (goto-scanner-state scanner-assign-state)))             
      ((or (char=? c #\+) (char=? c #\-) (char=? c #\*))
       (shift (emit cook-identifier)))
      ((char=? c #\^)
       (emit (cook-punctuation 'end-marker)))      
      (else (error "Invalid character to scan:" c)))))

(define scanner-identifier-state
  (lambda (c)
    (cond
      ((char-alphabetic? c)
       (shift (goto-scanner-state scanner-identifier-state)))
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-identifier-state)))
      (else (emit cook-identifier)))))

(define scanner-number-state
  (lambda (c)
    (cond
      ((char-numeric? c)
       (shift (goto-scanner-state scanner-number-state)))
      (else (emit cook-number)))))

(define scanner-comment-state
  (lambda (c)
    (cond
      ((char=? c #\newline)
       (drop (goto-scanner-state scanner-start-state)))
      (else (drop (goto-scanner-state scanner-comment-state))))))

(define scanner-assign-state
  (lambda (c)
    (cond
      ((char=? c #\=)
       (drop (emit (cook-punctuation 'assign-sym))))
      (else (emit (cook-punctuation 'colon))))))

(define scanner-ampersand-state
  (lambda (c)
    (cond
      ((char=? c #\&)
       (drop (emit (cook-punctuation 'double-ampersand))))
      (else (emit (cook-punctuation 'ampersand))))))

(define keywords-list
  '(if then else let in proc begin end letmutable letrecproc letrec
     during letdynamic letarray letproc local method define definearray
     simpleclass simpleinstance class instance abort letcont callcc
     coroutine wind unwind within sum))

;;; End of figures for Appendix E

;;; ****************************************************************

;;; Beginning of figures for Appendix F

;;; Figure F.1 : page 493

(define stream-car
  (lambda (s)
    (car (s))))

(define stream-cdr
  (lambda (s)
    (cdr (s))))

(define make-stream
  (lambda (car-val th)
    (lambda ()
      (cons car-val (let ((cdr-val #f))
                      (lambda ()
                        (if (pair? cdr-val)
                            cdr-val
                            (begin (set! cdr-val ((th))) cdr-val))))))))

(define the-null-stream
  (make-stream 'end-of-stream (lambda () the-null-stream)))

(define stream-null?
  (lambda (s)
    (eq? (stream-car s) 'end-of-stream)))

;;; Figure F.2 : page 494

(define stream-for-each
  (lambda (proc stream)
    (letrec
      ((loop
         (lambda (stream)
           (if (not (stream-null? stream))
               (begin
                 (proc (stream-car stream))
                 (loop (stream-cdr stream)))))))
      (loop stream))))

(define make-input-stream
  (lambda ()
    (let ((char (read-char)))
      (if (eof-object? char)
          the-null-stream
          (make-stream char make-input-stream)))))

;;; Figure F.3 : page 494

(define make-char-seq make-stream)

(define char-seq-head stream-car)

(define char-seq-tail stream-cdr)

(define make-token-seq make-stream)

(define token-seq-head stream-car)

(define token-seq-tail stream-cdr)

;;; Figure F.4 : page 495 

(define eval-print
  (lambda (tree)
    (write tree)))

(define eval-print
  (lambda (tree)
    (let ((result (eval-exp tree init-env)))
      (if (not (or (define? tree)
                   (definearray? tree)
                   (varassign? tree)
                   (arrayassign? tree)))
          (write result)))))

(define read-eval-print
  (lambda ()
    (display "--> ")
    (stream-for-each
      (lambda (tree)
        (eval-print tree)
        (newline)
        (display "--> "))
      (parse-token-seq parse-semicolon-terminated-form
        (scan-char-seq scanner-start-state (make-input-stream))))))

;;; Figure F.5 : page 496

(define parse-semicolon-terminated-form
  (lambda (token)
    (process-nt parse-form
      (goto-parser-state parse-terminated-by))))

(define parse-terminated-by
  (lambda (token)
    (case (token->class token)
      ((semicolon)
       (check/drop 'semicolon (reduce 'chain)))
      ((end)
       (reduce 'chain))
      (else (error "Invalid terminator:" token)))))

(define parse-token-seq
  (lambda (start-state token-seq)
    (let ((token (token-seq-head token-seq)))
      (if (or (eq? (token->class token) 'end)
              (eq? (token->class token) 'end-marker))
          the-null-stream
          (variant-case (parse-once start-state token-seq)
            (parser-answer (tree unparsed)
              (make-stream tree
                (lambda ()
                  (parse-token-seq start-state unparsed)))))))))

;;; End of figures for Appendix F

;;; ****************************************************************

;;; Beginning of figures for Appendix H

;;; Figure H.1 : page 502, 503

(define parse
  (lambda (exp)
    (if (form? exp)
        (parse-form exp)
        (parse-exp exp))))

(define form?
  (lambda (x)
    (and (pair? x) (memq (car x) '(define definearray)))))

(define parse-form
  (lambda (form)
    (let ((form-pair (assq (car form) form-table)))
      (if (and (pair? form-pair) (approve form))
          (apply (cdr form-pair) (cdr form))
          (error "Invalid top-level form:" form)))))

(define parse-exp
  (lambda (exp)
    (cond
      ((symbol? exp) (make-varref exp))
      ((number? exp) (make-lit exp))
      ((and (pair? exp) (not (form? exp))) (parse-pair exp))
      (else (error "Invalid expression:" exp)))))

(define parse-pair
  (lambda (exp)
    (let ((x (car exp)))
      (if (symbol? x)
          (let ((form-pair (assq x form-table)))
            (if (and (pair? form-pair) (approve exp))
                (apply (cdr form-pair) (cdr exp))
                (make-app (parse-exp (car exp)) (parse-rands (cdr exp)))))
          (make-app (parse-exp (car exp)) (parse-rands (cdr exp)))))))

(define approve
  (lambda (x) #t))

(define parse-decl
  (lambda (decl)
    (make-decl (car decl) (parse-exp (cadr decl)))))

(define parse-procdecl
  (lambda (triple)
    (make-procdecl (car triple) (cadr triple) (parse-exp (caddr triple)))))

(define parse-with-keys
  (lambda (proc)
    (letrec ((loop (lambda (exps)
                     (cond
                       ((null? exps) '())
                       ((and (pair? (car exps)) (eq? (car (car exps)) ':))
                        (map (lambda (decl)
                               (make-keydecl (cadr decl)
                                 (parse-exp (caddr decl))))
                             exps))
                       (else (cons (proc (car exps)) (loop (cdr exps))))))))
      loop)))

(define parse-rands (parse-with-keys parse))
(define parse-formals (parse-with-keys (lambda (x) x)))

;;; Figure H.2 : page 504, 505, 506

(define form-table
  (list
    (cons 'define
      (lambda (var exp)
        (make-define var (parse-exp exp))))
    (cons 'definearray
      (lambda (var exp)
        (make-definearray var (parse-exp exp))))
    (cons 'if
      (lambda (test-exp then-exp else-exp)
        (make-if (parse-exp test-exp) (parse-exp then-exp) (parse-exp else-exp))))
    (cons 'let
      (lambda (decls body)
        (make-let (map parse-decl decls) (parse-exp body))))
    (cons 'proc
      (lambda (formals body)
        (make-proc (parse-formals formals) (parse-exp body))))
    (cons 'varassign
      (lambda (var body)
        (make-varassign var (parse-exp body))))
    (cons 'letmutable
      (lambda (decls body)
        (make-letmutable (map parse-decl decls) (parse-exp body))))
    (cons 'begin
      (letrec ((loop (lambda (exps)
                       (if (null? (cdr exps))
                           (parse-exp (car exps))
                           (make-begin (parse-exp (car exps)) (loop (cdr exps)))))))
        (lambda exps
          (loop exps))))
    (cons 'letrecproc
      (lambda (procdecls body)
        (make-letrecproc (map parse-procdecl procdecls) (parse-exp body))))
    (cons 'letrec
      (lambda (decls body) (make-letrec (map parse-decl decls) (parse-exp body))))
    (cons 'dynassign
      (lambda (var exp body)
        (make-dynassign var (parse-exp exp) (parse-exp body))))
    (cons 'letdynamic
      (lambda (decls body)
        (make-letdynamic (map parse-decl decls) (parse-exp body))))))

(define form-table-for-chapter-6
  (list 
    (cons 'letarray
      (lambda (arraydecls body)
        (make-letarray (map parse-decl arraydecls) (parse-exp body))))
    (cons 'arrayref
      (lambda (array index)
        (make-arrayref (parse-exp array) (parse-exp index))))
    (cons 'arrayassign
      (lambda (array index exp)
        (make-arrayassign (parse-exp array) (parse-exp index) (parse-exp exp))))
    (cons 'letproc
      (lambda (procdecls body)
        (make-letproc (map parse-procdecl procdecls)
          (parse-exp body))))
    (cons 'local
      (lambda (decls body)
        (make-local (map parse-decl decls) (parse-exp body))))))

(define form-table-for-chapter-7
  (list
    (cons 'i-varref
      (lambda (var) (make-i-varref var)))
    (cons 'c-varref
      (lambda (var) (make-c-varref var)))
    (cons 'i-varassign
      (lambda (var exp) (make-i-varassign var (parse-exp exp))))
    (cons 'c-varassign
      (lambda (var exp) (make-c-varassign var (parse-exp exp))))
    (cons 'method
      (lambda (formals body)
        (make-method (parse-formals formals) (parse-exp body))))
    (cons 'meth-app
      (lambda args
        (let ((name (car args)) (rands (cdr args)))
          (if (and (pair? rands) (symbol? (car rands)) (eq? (car rands) 'super))
              (make-super-meth-app name (parse-rands (cdr rands)))
              (make-meth-app name (parse-rands rands))))))
    (cons 'simpleinstance
      (lambda (class-exp)
        (make-new-simpleinst (parse-exp class-exp))))
    (cons 'simpleclass
      (lambda (c-vars i-vars methdecls init-exp)
        (make-new-simpleclass c-vars i-vars
          (map parse-decl methdecls)
          (parse-exp init-exp))))
    (cons 'class
      (lambda (parent-exp c-vars i-vars methdecls init-exp)
        (make-new-class (parse-exp parent-exp) c-vars i-vars
          (map parse-decl methdecls)
          (parse-exp init-exp))))
    (cons 'instance
      (lambda (class-exp parent-exp i-vars methdecls)
        (make-new-instance (parse-exp class-exp) (parse-exp parent-exp)
          i-vars
          (map parse-decl methdecls))))))

(define form-table-for-chapter-9
  (list     
    (cons 'abort
      (lambda (exp) (make-abort (parse-exp exp))))
    (cons 'letcont
      (lambda (var body) (make-letcont var (parse-exp body))))
    (cons 'callcc
      (lambda (exp) (make-callcc (parse-exp exp))))
    (cons 'coroutine
      (lambda (exp) (make-coroutine (parse-exp exp))))
    (cons 'wind
      (lambda (pre body post)
        (make-wind (parse-exp pre) (parse-exp body) (parse-exp post))))))

(define form-table-for-chapter-12
  (list
    (cons 'sum
      (lambda rands
        (make-sum (parse-rands rands))))))

;;; End of figures for Appendix H

