;;; top.s

;;; user-level entry points

(define sllgen:make-string-parser        
  (lambda (scanner-spec grammar)
    (let ((parser (sllgen:make-parser grammar))
          (scanner (sllgen:make-scanner
                    (append
                     (sllgen:grammar->string-literal-scanner-spec
                      grammar)
                     scanner-spec))))
      (lambda (string)
        (let* ((char-stream (sllgen:string->stream string))
               (token-stream (scanner char-stream))
               (last-line (sllgen:char-stream->location char-stream)))
          (parser 
           (sllgen:stream-add-sentinel-via-thunk
            token-stream
            (lambda ()
              (sllgen:make-token 'end-marker #f
                                 (sllgen:char-stream->location char-stream))))
           (lambda (tree token token-stream)
             (if (null? token)
                 (sllgen:stream-get! token-stream
                                     (lambda (tok1 str1)
                                       (set! token tok1)
                                       (set! token-stream str1))
                                     (lambda ()
                                       (sllgen:error 'sllgen:string-parser
                                                     "~%Internal error: shouldn't run off end of stream with sentinels"))))
             (if (eq? (sllgen:token->class token) 'end-marker)
                 tree
                 (sllgen:error 'parsing
                               "at line ~s~%Symbols left over: ~s ~s etc..."
                               (sllgen:token->location token)
                               (sllgen:token->class token)
                               (sllgen:token->data token))))))))))

(define sllgen:make-stream-scanner
  (lambda (scanner-spec grammar)
    (sllgen:make-scanner
     (append
      (sllgen:grammar->string-literal-scanner-spec
       grammar)
      scanner-spec))))

(define sllgen:make-string-scanner
  (lambda (scanner-spec grammar)
    (let ((scanner (sllgen:make-stream-scanner scanner-spec grammar)))
      (lambda (string)
        (sllgen:stream->list
         (scanner (sllgen:string->stream string)))))))

(define sllgen:make-define-datatypes
  (lambda (scanner-spec grammar)
    (let ((datatype-definitions
           (sllgen:build-define-datatype-definitions scanner-spec grammar)))
      ;(sllgen:pretty-print datatype-definitions)
      (for-each
       (lambda (dd) (eval dd (interaction-environment)))
       datatype-definitions))))

(define sllgen:show-define-datatypes
  (lambda (scanner-spec grammar)
    (let ((datatype-definitions
           (sllgen:build-define-datatype-definitions scanner-spec grammar)))
      (for-each
       sllgen:pretty-print
       datatype-definitions))))

(define sllgen:list-define-datatypes
  (lambda (scanner-spec grammar)
    (sllgen:build-define-datatype-definitions scanner-spec grammar)))

(define sllgen:make-stream-parser        
  (lambda (scanner-spec grammar)
    (let ((parser (sllgen:make-parser grammar))
          (scanner (sllgen:make-scanner
                    (append
                     (sllgen:grammar->string-literal-scanner-spec
                      grammar)
                     scanner-spec))))
      (lambda (char-stream)
        (let ((stream
               (sllgen:stream-add-sentinel-via-thunk
                (scanner char-stream)
                (lambda ()
                  (sllgen:make-token 'end-marker #f
                                     (sllgen:char-stream->location char-stream))))))
          (let loop ((stream stream))
            (lambda (fn eos)
              ((parser stream
                       (lambda (tree token token-stream)
                         (sllgen:make-stream 'tag1
                                             tree
                                             (lambda (fn eos)   ; prevent evaluation for now
                                               ((loop 
                                                 ;; push the lookahead token back on the
                                                 ;; stream iff it's there.
                                                 (if (null? token)
                                                     token-stream
                                                     (sllgen:make-stream 'tag2 token token-stream)))
                                                fn eos)))))
               fn eos))))))))

(define sllgen:make-rep-loop
  (lambda (prompt eval-fn stream-parser)
    (lambda ()
      (display prompt)
      (let loop
          ((ast-stream (stream-parser (sllgen:stdin-char-stream))))
        ;; these seem not to make a difference
        ;; (flush-output-port)               ; chez
        ;; (flush-output)                    ; dr scheme
        (sllgen:stream-get! ast-stream
                            (lambda (tree stream)
                              (write (eval-fn tree))
                              (newline)
                              (display prompt)
                              (loop stream))
                            (lambda () #t))))))
