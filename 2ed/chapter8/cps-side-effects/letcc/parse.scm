;; grammar

(define lex7
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number   (digit (arbno digit)) number)))

;; language of section 3.6, plus letcc.

(define gram7
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression ("(" expression (arbno expression) ")") app-exp)
    (expression                         ; 3-6
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)
    (expression ("letcc" identifier "in" expression) letcc-exp) ; new
    (expression ("throw" expression "to" expression) throw-exp) ; new in 9-2
; no set
;     (expression ("set" identifier "=" expression) varassign-exp)

;; make primitives match unparse.
    (primitive ("+")    plus-prim)
    (primitive ("-")    minus-prim)
    (primitive ("*")    mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("zero?") zero?-prim)
    (primitive ("car")  car-prim)
    (primitive ("cdr")  cdr-prim)    
    (primitive ("cons") cons-prim)
    (primitive ("emptylist") empty-prim) ; ??
    (primitive ("null?") null?-prim)
    (primitive ("list")  list-prim)
    ))

(sllgen:make-define-datatypes lex7 gram7)

(define scan&parse
  (sllgen:make-string-parser lex7 gram7))

(define just-scan
  (sllgen:make-string-scanner lex7 gram7))

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lex7 gram7)))
