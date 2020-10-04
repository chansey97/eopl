;;; ****************************************************************        

;;; scan.scm

;;; Scanner based on regexps and longest-match property

;; new version using proper lookahead in sllgen:scanner-inner-loop
;; Tue Dec 01 11:42:53 1998

;;; External syntax of scanner:

;;; scanner ::= (init-state ...)
;;; init-state ::= (classname (regexp ...) action-opcode)
;;; regexp = etester | (or regexp ...) | (arbno regexp) 
;;;        | (concat regexp ...)
;;; etester ::= string | LETTER | DIGIT | WHITESPACE | ANY | (NOT char)

;; top level stream transducer:

(define sllgen:make-scanner
  (lambda (init-states)
    (let ((start-states (sllgen:parse-scanner-spec init-states)))
      (lambda (input-stream)
        (sllgen:scanner-outer-loop start-states input-stream)))))

;;; Conversion of external to internal rep

(define sllgen:parse-scanner-spec
  (lambda (init-states)
    (map sllgen:parse-init-state init-states)))

(define sllgen:parse-init-state
  (lambda (init-state)
    (sllgen:check-syntax-init-state init-state)
    (let ((classname (car init-state))
          (regexps (cadr init-state))
          (opcode (caddr init-state)))
      (sllgen:make-local-state
       (map sllgen:parse-regexp regexps)
       (cons opcode classname)))))

(define sllgen:check-syntax-init-state
  (lambda (v)
    (or
     (and
      (list? v) 
      (= (length v) 3) 
      (symbol? (car v))
      (list? (cadr v))
      (symbol? (caddr v))
      (member (caddr v) sllgen:action-preference-list))
     (sllgen:error 'scanner-generation "bad scanner item ~s" v))))

(define sllgen:parse-regexp
  (lambda (regexp)
    (cond
      ((char? regexp) (sllgen:make-tester-regexp regexp))
      ((string? regexp) (sllgen:string->regexp regexp))
      ((symbol? regexp) (sllgen:symbol->regexp regexp))
      ((and (pair? regexp) (symbol? (car regexp)))
       (case (car regexp)
         ((or)
          (sllgen:make-or-regexp (map sllgen:parse-regexp (cdr regexp))))
         ((concat)
          (sllgen:make-concat-regexp (map sllgen:parse-regexp (cdr regexp))))
         ((arbno)
          (and
           (or (pair? (cdr regexp))
               (sllgen:error 'scanner-generation "bad regexp ~s" regexp))
           (sllgen:make-arbno-regexp (sllgen:parse-regexp (cadr regexp)))))
         ((not) (and
                 (or (and (pair? (cdr regexp))
                          (char? (cadr regexp)))
                     (sllgen:error 'sllgen:parse-regexp "bad regexp ~s" regexp))
                 (sllgen:make-tester-regexp regexp)))))
      (else (sllgen:error 'scanner-generation "bad regexp ~s" regexp)))))

(define sllgen:string->regexp
  (lambda (string)
    (sllgen:make-concat-regexp
     (map sllgen:make-tester-regexp
          (map sllgen:make-char-tester (string->list string))))))

(define sllgen:symbol->regexp
  (lambda (sym)
    (if (member sym sllgen:tester-symbol-list)
        (sllgen:make-tester-regexp sym)
        (sllgen:error 'scanner-generation "unknown tester ~s" sym))))

;;; regexps 
;;; regexp = tester | (or regexp ...) | (arbno regexp) 
;;;        | (concat regexp ...)


; (define-datatype regexp
;   (tester-regexp sllgen:tester?)
;   (or-regexp (list-of regexp?))
;   (arbno-regexp regexp?)
;   (concat-regexp (list-of regexp?)))

(define sllgen:make-tester-regexp (lambda (x) x))
(define sllgen:make-or-regexp (lambda (res) (cons 'or res)))
(define sllgen:make-arbno-regexp (lambda (re) (list 'arbno re)))
(define sllgen:make-concat-regexp (lambda (rs) (cons 'concat rs)))

(define sllgen:tester-regexp? 
  (lambda (x) 
    (and (sllgen:tester? x) (lambda (f) (f x)))))

(define sllgen:or-regexp?
  (lambda (x)
    (and (eq? (car x) 'or)
         (lambda (f) (f (cdr x))))))

(define sllgen:arbno-regexp?
  (lambda (x)
    (and (eq? (car x) 'arbno)
         (lambda (f) (f (cadr x))))))

(define sllgen:concat-regexp?
  (lambda (x)
    (and (eq? (car x) 'concat)
         (lambda (f) (f (cdr x))))))

;; (sllgen:select-variant obj selector1 receiver1 ... [err-thunk])

(define sllgen:select-variant
  (lambda (obj . alts)
    (let loop ((alts alts))
      (cond
        ((null? alts)
         (sllgen:error 'sllgen:select-variant
                       "~%Internal error: nothing matched ~a" obj))
        ((null? (cdr alts)) ((car alts)))
        (((car alts) obj) => (lambda (f) (f (cadr alts))))
        (else (loop (cddr alts)))))))


(define sllgen:unparse-regexp                  ; deals with regexps or actions
  (lambda (regexp)
    (if (sllgen:action? regexp)
        regexp
        (sllgen:select-variant regexp
                               sllgen:tester-regexp?
                               (lambda (tester) tester)
                               sllgen:arbno-regexp?
                               (lambda (regexp)
                                 (list 'arbno (sllgen:unparse-regexp regexp)))
                               sllgen:concat-regexp?
                               (lambda (regexps)
                                 (cons 'concat (map sllgen:unparse-regexp regexps)))
                               sllgen:or-regexp?
                               (lambda (regexps)
                                 (cons 'or (map sllgen:unparse-regexp regexps)))))))

;;; testers
;;; tester ::= char | LETTER | DIGIT | ANY | WHITESPACE | (NOT char)

(define sllgen:tester-symbol-list '(letter digit any whitespace))

(define sllgen:apply-tester
  (lambda (tester ch)
    (cond
      ((char? tester) (char=? tester ch))
      ((symbol? tester)
       (case tester
         ((whitespace) (char-whitespace? ch))
         ((letter) (char-alphabetic? ch))
         ((digit) (char-numeric? ch))
         ((any) #t)                    ; ELSE is not really a tester
         (else (sllgen:error 'sllgen:apply-tester
                             "~%Internal error: unknown tester ~s" tester))))
      ((eq? (car tester) 'not)
       (not (char=? (cadr tester) ch)))
      (else (sllgen:error 'sllgen:apply-tester
                          "~%Internal error: unknown tester ~s"
                          tester)))))

(define sllgen:make-char-tester 
  (lambda (char) 
    (and (or (char? char)
             (sllgen:error 'scanner-generation "illegal character ~s" char))
         char)))

(define sllgen:tester?
  (lambda (v)
    (or (char? v)
        (member v sllgen:tester-symbol-list)
        (and (pair? v)
             (eq? (car v) 'not)
             (pair? (cdr v))
             (char? (cadr v))))))


;;; actions
;;; action        ::= (action-opcode . classname)
;;; action-opcode :: = skip | symbol | number | string

;;; make-symbol, make-number, and make-string are supported
;;; alternates, but are deprecated.

;;; the classname becomes the the name of token.

;; if multiple actions are possible, do the one that appears here
;; first.  make-string is first, so literal strings trump identifiers.

(define sllgen:action-preference-list
  '(make-string make-symbol make-number skip))

(define sllgen:action-preference-list
  '(string make-string symbol make-symbol number make-number skip))

(define sllgen:find-preferred-action
  (lambda (action-opcodes)
    (let loop ((preferences sllgen:action-preference-list))
      (cond
        ((null? preferences)
         (sllgen:error 'scanning "~%No known actions in ~s"
                       action-opcodes))
        ((member (car preferences) action-opcodes)
         (car preferences))
        (else (loop (cdr preferences)))))))

(define sllgen:is-all-skip?
  (lambda (actions)
    (let ((opcode  (sllgen:find-preferred-action (map car actions))))
      (eq? opcode 'skip))))

(define sllgen:cook-token
  (lambda (buffer actions loc)
    (let* ((opcode (sllgen:find-preferred-action (map car actions)))
           (classname (cdr (assq opcode actions))))
      (case opcode
        ((skip) (sllgen:error 'sllgen:cook-token
                              "~%Internal error: skip should have been handled earlier ~s"
                              actions))
        ((make-symbol symbol) 
         (sllgen:make-token classname
                            (string->symbol (list->string (reverse buffer)))
                            loc))
        ((make-number number)
         (sllgen:make-token classname
                            (string->number (list->string (reverse buffer)))
                            loc))
        ((make-string string)
         (sllgen:make-token classname
                            (list->string (reverse buffer))
                            loc))
        (else
         (sllgen:error 'scanning
                       "~%Unknown opcode selected from action list ~s"
                       actions))))))

; (define sllgen:cook-token
;   (lambda (buffer actions loc)
;     (let* ((opcode (sllgen:find-preferred-action (map car actions)))
;            ;; (classname (cdr (assq opcode actions)))
;            )
;       (case opcode
;         ((skip) (sllgen:error 'sllgen:cook-token
;                   "~%Internal error: skip should have been handled earlier ~s"
;                   actions))
;         ((make-symbol identifier) 
;          (sllgen:make-token 'identifier
;            (string->symbol (list->string (reverse buffer)))
;            loc))
;         ((make-number number)
;          (sllgen:make-token 'number
;            (string->number (list->string (reverse buffer)))
;            loc))
;         ((make-string string)
;          (sllgen:make-token 'string
;            (list->string (reverse buffer))
;            loc))
;         (else
;           (sllgen:error 'scanning
;             "~%Unknown opcode selected from action list ~s"
;             actions))))))

(define sllgen:action?
  (lambda (action)
    (and
     (pair? action)
     (member (car action) sllgen:action-preference-list)
     (symbol? (cdr action))))) 

;; tokens

; (define-record token (symbol? (lambda (v) #t)))
(define sllgen:make-token list)

;;; localstate = regexp* x action

; (define-record local-state ((list-of regexp?) sllgen:action?))

(define sllgen:make-local-state 
  (lambda (regexps action)
    (append regexps (list action))))

;; k = (actions * newstates * char * stream) -> val
(define sllgen:scanner-inner-loop
  (lambda (local-states stream k)            
    (let ((actions '())
          (newstates '())
          (char '())
          (eos-found? #f))              ; do we need to return this too?
      ;(eopl:printf "initializing sllgen:scanner-inner-loop~%")
      (let loop ((local-states local-states)) ; local-states
        ;         '(begin
        ;            (eopl:printf "sllgen:scanner-inner-loop char = ~s actions=~s local-states =~%"
        ;              char actions)
        ;            (for-each
        ;              (lambda (local-state)
        ;                (sllgen:pretty-print (map sllgen:unparse-regexp local-state)))
        ;              local-states)
        ;            (eopl:printf "newstates = ~%")
        ;            (for-each
        ;              (lambda (local-state)
        ;                (sllgen:pretty-print (map sllgen:unparse-regexp local-state)))
        ;              newstates))
        (if (null? local-states)
            ;; no more states to consider
            (begin
              ;             '(eopl:printf
              ;               "sllgen:scanner-inner-loop returning with actions = ~s char = ~s newstates = ~%"
              ;               actions char)
              ;             '(for-each
              ;               (lambda (local-state)
              ;                 (sllgen:pretty-print (map sllgen:unparse-regexp local-state)))
              ;               newstates)
              (k actions newstates char stream))
            (let ((state        (car local-states)))
              ;           (eopl:printf "first state:~%")
              ;           (sllgen:pretty-print state)
              (cond
                ((sllgen:action? (car state))    ; state should never be null
                 ;; recommend accepting what's in the buffer
                 (set! actions (cons (car state) actions))
                 (loop (cdr local-states)))
                ((sllgen:tester-regexp? (car state))
                 =>
                 (sllgen:xapply
                  (lambda (tester)
                    ;; get a character if one hasn't been gotten and we
                    ;; haven't discovered eos.
                    (if (and (null? char) (not eos-found?))
                        (sllgen:char-stream-get! stream
                                                 (lambda (ch1)
                                                   '(eopl:printf "read character ~s~%" ch1)
                                                   (set! char ch1))
                                                 (lambda ()
                                                   (set! eos-found? #t))))
                    '(eopl:printf "applying tester ~s to ~s~%" tester char)
                    (if (and (not (null? char))
                             (sllgen:apply-tester tester char))
                        ;; passed the test -- shift is possible
                        (set! newstates (cons (cdr state) newstates)))
                    ;; either way, continue with the other local-states
                    (loop (cdr local-states)))))
                ((sllgen:or-regexp? (car state))
                 =>
                 (sllgen:xapply
                  (lambda (alternatives)
                    ;; its ((or alts) regexps action)
                    (loop (append
                           (map (lambda (alt) (cons alt (cdr state)))
                                alternatives)
                           (cdr local-states))))))
                ((sllgen:arbno-regexp? (car state))
                 =>
                 (sllgen:xapply
                  (lambda (regexp1)
                    ;; it's ((arbno regexp1) regexps action)
                    ;; so its either (regexps action) or
                    ;; (regexp1 (arbno regexp1) regexps action)
                    (loop
                     (append
                      (list 
                       (cdr state)    ; 0 occurrences
                       (cons regexp1 state) ; >= 1 occurrences
                       )
                      (cdr local-states))))))
                ((sllgen:concat-regexp? (car state))
                 =>
                 (sllgen:xapply
                  (lambda (sequents)
                    ;; (printf "processing concat: sequents = ~s~%" sequents)
                    (loop
                     (cons 
                         (append sequents (cdr state))
                       (cdr local-states)))))))))))))

(define sllgen:xapply (lambda (x) (lambda (y) (y x))))

(define sllgen:scanner-outer-loop
  (lambda (start-states input-stream)   ; -> (token stream), same as before
    (let 
        ((states start-states)            ; list of local-states
         (buffer '())                     ; characters accumulated so far
         (success-buffer '())             ; characters for the last
         ; candidate token (a sublist
         ; of buffer)
         (actions '())                    ; actions we might perform on succ-buff
         (stream input-stream)
         )
      (letrec
          ((process-stream                
            (lambda ()
              (sllgen:scanner-inner-loop states stream
                                         (lambda (new-actions new-states char new-stream)
                                           (if (not (null? new-actions))
                                               ;; ok, the current buffer is a candidate token
                                               (begin
                                                 (set! success-buffer buffer)
                                                 ;; (printf "success-buffer =~s~%" success-buffer)
                                                 (set! actions new-actions))
                                               ;; otherwise leave success-buffer and actions alone
                                               )
                                           (if (null? new-states)
                                               ;; we are definitely at the end of this token
                                               (process-buffer char new-stream)
                                               ;; there might be more -- absorb another character and
                                               ;; consider what to do next.
                                               (begin
                                                 (set! buffer (cons char buffer))
                                                 (set! stream  new-stream)
                                                 (set! states new-states)
                                                 (process-stream)))))))
           (process-buffer                ; can't absorb any more chars,
            ; better make do with what we have.
            (lambda (char new-stream)
              ;; first, push the lookahead character back on the
              ;; stream.
              (if (not (null? char))
                  (sllgen:char-stream-push-back! char new-stream))
              (set! stream new-stream)
              (if (null? buffer)
                  ;; any characters in the buffer?  If not, the stream
                  ;; must have been empty, so return the empty stream.
                  sllgen:empty-stream
                  ;; otherwise, push back any unused characters into the stream
                  (begin
                    (let push-back-loop ()
                      (if (eq? buffer success-buffer)
                          ;; this really is reference equality.
                          #t
                          (begin
                            ;; (eopl:printf "pushing back ~s~%" (car buff))
                            (sllgen:char-stream-push-back! (car buffer) stream)
                            (set! buffer (cdr buffer))
                            (push-back-loop))))
                    ;; next, look at actions. 
                    (cond
                      ((null? actions)
                       ;; no actions possible?  Must be a mistake
                       (sllgen:error 'scanning
                                     "~%No actions found for ~s" (reverse buffer)))
                      ((sllgen:is-all-skip? actions)
                       ;; If only action is SKIP, 
                       ;; then discard buffer and start again. 
                       (set! buffer '())
                       (set! success-buffer '())
                       (set! states start-states) ;!
                       (process-stream))
                      ;;    Otherwise, perform action on the success-buffer
                      ;;    and create a token stream.
                      (else
                       (let ((token
                              (sllgen:cook-token
                               success-buffer
                               actions
                               (sllgen:char-stream->location stream))))
                         (sllgen:make-stream 'tag5
                                             token
                                             (lambda (fcn eos-fcn)
                                               ((sllgen:scanner-outer-loop start-states stream)
                                                fcn eos-fcn)))))))))))
        ;; start by trying to absorb a character
        (process-stream)))))

;; Watch out for examples like:
;; ("a" | "b" | "c" | "abcdef")  matched against "abc" should produce
;; 3 tokens before reaching eos.

;; tokens

; (define-record token (symbol? (lambda (v) #t)))

(define sllgen:make-token list)
(define sllgen:token->class car)
(define sllgen:token->data cadr)
(define sllgen:token->location caddr)

;;; streams

;;; (sllgen:stream-get! (sllgen:make-stream tag char stream) fcn eos-fcn) = (fcn char stream)

;;; this is banged, because doing it on some streams may cause a side-effect. 
(define sllgen:stream-get!
  (lambda (str fcn eos-fcn) 
    (str fcn eos-fcn)))

(define sllgen:empty-stream
  (lambda (fcn eos-fcn)
    (eos-fcn)))

(define sllgen:make-stream
  (lambda (tag char stream)
    ;(eopl:printf "sllgen:make-stream: building stream at ~s with ~s~%" tag char)
    (lambda (fcn eos-fcn)
      ;(eopl:printf "sllgen:make-stream: emitting ~s~%" char)
      (fcn char stream))))

(define sllgen:list->stream
  (lambda (l)
    (if (null? l) sllgen:empty-stream
        (sllgen:make-stream 'sllgen:list->stream (car l) (sllgen:list->stream (cdr l))))))

; ;; brute force for now.
; (define sllgen:string->stream
;   (lambda (string) (sllgen:list->stream (string->list string))))

; ;; this one has state:
; (define sllgen:stdin-char-stream
;   (lambda (fcn eos-fcn)
;     (let ((char (read-char)))
;       (if (eof-object? char)           
;         (eos-fcn)
;         (fcn char sllgen:stdin-char-stream)))))

(define sllgen:stream->list
  (lambda (stream)
    (sllgen:stream-get! stream
                        (lambda (val stream)
                          (cons val (sllgen:stream->list stream)))
                        (lambda () '()))))

(define sllgen:constant-stream
  (lambda (val)
    (lambda (fn eos)
      (fn val (sllgen:constant-stream val)))))

;; takes a stream and produces another stream that produces the
;; sentinel instead of an end-of-stream
(define sllgen:stream-add-sentinel
  (lambda (stream sentinel)
    (lambda (fn eos)                    ; here's what to do on a get
      (sllgen:stream-get! stream 
                          (lambda (val str)
                            (fn val (sllgen:stream-add-sentinel str sentinel)))
                          (lambda ()
                            (fn sentinel (sllgen:constant-stream sentinel)))))))

(define sllgen:stream-add-sentinel-via-thunk
  (lambda (stream sentinel-fcn)
    (lambda (fn eos)                    ; here's what to do on a get
      (sllgen:stream-get! stream 
                          (lambda (val str)
                            (fn val (sllgen:stream-add-sentinel-via-thunk str sentinel-fcn)))
                          (lambda ()
                            (fn (sentinel-fcn) (sllgen:constant-stream (sentinel-fcn))))))))

(define sllgen:stream-add-sentinel-via-thunk
  (lambda (stream sentinel-fcn)
    (lambda (fn eos)                    ; here's what to do on a get
      (sllgen:stream-get! stream 
                          (lambda (val str)
                            (fn val (sllgen:stream-add-sentinel-via-thunk str sentinel-fcn)))
                          (lambda ()                    
                            ;; when the stream runs out, try this
                            (let ((sentinel (sentinel-fcn)))
                              ;            (eopl:printf "~s~%" sentinel)
                              (fn sentinel (sllgen:constant-stream sentinel))))))))

; no longer used
; (define sllgen:stream-get
;   (lambda (stream fcn)
;     (sllgen:stream-get! stream fcn
;       (lambda ()
;         (sllgen:error 'sllgen:stream-get
;           "internal error: old streams aren't supposed to produce eos")))))


;;; ****************************************************************

;;; imperative character streams Tue Apr 11 12:09:32 2000

;;; interface:

;;; sllgen:string->stream  : string -> charstream
;;; sllgen:stdin-char-stream : () -> charstream

;;; sllgen:char-stream-get! : !charstream * (char -> ans) * (() -> ans)
;;;                             -> ans
;;;                           [modifies charstream]
;;; sllgen:char-stream-push-back! : char * !charstream -> ()
;;; sllgen:char-stream->location : charstream -> location

;;; for the moment, a location is a line number

;;; we have two kinds of streams-- those built by string->stream and
;;; those built by stdin-char-stream.  We'll use a little OO here.

;;; represent by a vector

;;; [get-fn ; push-back-fn ; location ; other stuff]

(define sllgen:char-stream-get!
  (lambda (cstr sk th)
    ((vector-ref cstr 0) cstr sk th)))

(define sllgen:char-stream-push-back!
  (lambda (ch cstr)
    ((vector-ref cstr 1) ch cstr)))

(define sllgen:char-stream->location
  (lambda (cstr)
    (vector-ref cstr 2)))

(define sllgen:set-location!
  (lambda (vec val)
    (vector-set! vec 2 val)))

;;; for a string-built stream, the other stuff consists of an index
;;; into the string  for the next unread character, and a string.

(define sllgen:string->stream
  (lambda (string)
    (let ((len (string-length string)))
      (vector
       ;; the get! function
       (lambda (vec sk th)
         (let ((index (vector-ref vec 3)))
           (if (>= index len)
               (th)
               (begin
                 (vector-set! vec 3 (+ 1 index))
                 (let ((ch (string-ref (vector-ref vec 4) index)))
                   (sllgen:set-location! vec
                                         (sllgen:increment-location ch
                                                                    (sllgen:char-stream->location vec)))
                   (sk ch))))))
       ;; the push-back function
       (lambda (ch vec)
         (sllgen:set-location! vec
                               (sllgen:decrement-location ch
                                                          (sllgen:char-stream->location vec)))
         (vector-set! vec 3 (- (vector-ref vec 3) 1)))
       ;; the location is initially 1
       1
       ;; the index is initially 0
       0
       string                          ;; the string
       ))))


(define sllgen:stdin-char-stream
  (lambda (fcn eos-fcn)
    (let ((char (read-char)))
      (if (eof-object? char)           
          (eos-fcn)
          (fcn char sllgen:stdin-char-stream)))))

;; for stdin-char-stream, we have
;; [get-fn ; push-back-fn ; location ; push-back stack]

(define sllgen:stdin-char-stream        ; this must be a thunk to reset the
  ;  line number
  (lambda ()
    (vector
     ;; the get! fcn
     (lambda (vec sk th)
       (let ((read-back-stack (vector-ref vec 3)))
         (if (null? read-back-stack)
             (let ((char (read-char)))
               (if (eof-object? char)
                   (th)
                   (begin
                     (sllgen:set-location! vec
                                           (sllgen:increment-location char
                                                                      (sllgen:char-stream->location vec)))
                     (sk char))))
             (let ((char (car read-back-stack)))
               (sllgen:set-location! vec
                                     (sllgen:increment-location char
                                                                (sllgen:char-stream->location vec)))
               (vector-set! vec 3 (cdr read-back-stack))
               (sk char)))))
     ;; the push back
     (lambda (ch vec)
       (sllgen:set-location! vec
                             (sllgen:decrement-location ch
                                                        (sllgen:char-stream->location vec)))
       (vector-set! vec 3 (cons ch (vector-ref vec 3))))
     0                                 ; location is initially 0 to
     ;  swallow the initial newline
     '()                               ; push-back is initially empty
     )))

(define sllgen:char-stream->list
  (lambda (cstr)
    (let loop ()
      (sllgen:char-stream-get! cstr
                               (lambda (ch) (cons ch (loop)))
                               (lambda () '())))))

(define sllgen:char-stream->list2
  (lambda (cstr)
    (let loop ()
      (sllgen:char-stream-get! cstr
                               (lambda (ch)
                                 (cons
                                     (cons ch (sllgen:char-stream->location cstr))
                                   (loop)))
                               (lambda () '())))))


(define sllgen:increment-location
  (lambda (ch n)
    (if (eqv? ch #\newline) (+ 1 n) n)))

(define sllgen:decrement-location
  (lambda (ch n)
    (if (eqv? ch #\newline) (- n 1) n)))

;;; see tests.s for examples.
