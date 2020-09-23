(load "../../poor-man-module.scm")
(load-module "../../" "r5rs" )
(load-module "../../" "sllgen")
(load-module "../../" "test-harness")

;;; ****************************************************************
;;; ****************************************************************

;;; sample input  (test-sllgen2.scm)

; (define lex2
;    '((whitespace (whitespace) skip)
;      (comment ("%" (arbno (not #\newline))) skip)
;      (identifier (letter (arbno (or letter digit))) make-symbol)
;      (number (digit (arbno digit)) make-number)))

; (define gram2
;   '((expression
;       (number)
;       lit-exp)
;     (expression
;       (identifier)
;       var-exp)
;     (expression
;       ("let" (separated-list declaration ";") "in" expression)
;       let-exp)
;     (expression
;       ("(" expression (arbno expression) ")")
;       app-exp)
;     (declaration
;       (identifier "=" expression)
;       decl)))

; (define test2 '*)
; (define scan2 '*)

; (define gen2
;   (lambda ()
;     ; if define-datatypes is loaded:
;    ; (sllgen:generate-define-datatypes lex2 gram2)
;     (set! test2
;       (let ((parser (sllgen:make-string-parser lex2 gram2)))
;         (lambda (string)
;           (sllgen:pretty-print (parser string)))))
;     (set! scan2
;       (sllgen:make-string-scanner lex2 gram2))))

; (define rep
;   (lambda ()
;     (sllgen:make-rep-loop "--> " (lambda (x) (sllgen:pretty-print x) #t)
;       (sllgen:make-stream-parser lex2 gram2))))

; (define rsp
;   (lambda ()
;     (sllgen:make-rep-loop "--> " (lambda (x) x)
;       (sllgen:make-stream-scanner lex2 gram2))))


; Welcome to MzScheme version 53, Copyright (c) 1995-98 PLT (Matthew Flatt)
; > (load "sllgen.scm")
; sllgen.scm 2.3.4 Time-stamp: <1998-12-31 15:35:02 Mitch>
; > (load "test-sllgen2.scm")
; > (gen2)
; > (test2 "let x = 3; y=4 in (f x y)")
; (let-exp
;   ((decl x (lit-exp 3)) (decl y (lit-exp 4)))
;   (app-exp (var-exp f) ((var-exp x) (var-exp y))))
; > (rep)
; --> x
; (var-exp x)
; #t
; --> (f y)
; (app-exp (var-exp f) ((var-exp y)))
; #t
; --> user brek
; > (sllgen:list-define-datatypes lex2 gram2)
; (define-datatype
;   expression
;   expression?
;   expression-case
;   (lit-exp mae-lit-exp (lit-exp39 number?))
;   (var-exp make-var-exp (var-exp40 symbol?))
;   (let-exp
;     make-let-exp
;     (let-exp41 (list-of declaration?))
;     (let-exp42 expression?))
;   (app-exp
;     make-app-exp
;     (app-exp43 expression?)
;     (app-exp44 (list-of expression?))))
; (define-datatype
;   declaration
;   declaration?
;   declaration-case
;   (decl make-decl (decl45 symbol?) (decl46 expression?)))
; > 

; sample input  (test-sllgen2.scm)

(define lex2
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) make-symbol)
    (number (digit (arbno digit)) make-number)))

(define gram2
  '((expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("let" (arbno declaration) "in" expression)
     let-exp)
    (expression
     ("mvlet"
      (separated-list  (separated-list identifier ",")
                       "=" expression ";") "in" expression)
     lets-exp)
    (expression
     ("(" expression (arbno expression) ")")
     app-exp)
    (declaration
     (identifier "=" expression)
     decl)))

; (sllgen:make-define-datatypes lex2 gram2)

(define show2
  (lambda () (sllgen:list-define-datatypes lex2 gram2)))

(define parse2 '*)

(define gen2
  (lambda () (set! parse2 (sllgen:make-string-parser lex2 gram2))))

(define gram3
  '((expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)
    (expression
     ("mvlet" (arbno (arbno identifier) "=" expression) "in" expression)
     mvlet-exp)
    (expression
     ("(" expression (arbno expression) ")")
     app-exp)
    (declaration
     (identifier "=" (arbno expression))
     decl)))

(define show3
  (lambda () (sllgen:list-define-datatypes lex2 gram3)))

(define list3
  (lambda () (sllgen:list-define-datatypes lex2 gram3)))

(define parse3 '*)

(define gen3
  (lambda () (set! parse3 (sllgen:make-string-parser lex2 gram3))))


(define gram4
  '((expression
     ("let" (separated-list (separated-list identifier "," ) "=" expression ";" ) 
            "in" expression)
     let-exp)
    (expression
     (number)
     lit-exp)))

(define parse4 '*)

(define gen4
  (lambda () (set! parse4 (sllgen:make-string-parser lex2 gram4))))

(define show4
  (lambda () (sllgen:list-define-datatypes lex2 gram4)))
