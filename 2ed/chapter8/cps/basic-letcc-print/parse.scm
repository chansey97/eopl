;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number   (digit (arbno digit)) number)))

;; language of section 3.6

(define the-grammar
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
    (expression                         
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)

;;; print, in source and target langs:
    (expression ("print" "(" expression ")") print-exp)
    (expression ("printc" "(" expression "," expression ")" ) printc-exp)

;;; letcc and throw
    (expression ("letcc" identifier "in" expression) letcc-exp) 
    (expression ("throw" expression "to" expression) throw-exp) 

; no set
;     (expression ("set" identifier "=" expression) varassign-exp)

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

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
