; Scheme Interpreter
; 2015-04
; Nakano Mizuki

; Global environment
(define GLOBAL-ENV
  (list
    (list 'number? 'primitive number?)
    (list '+ 'primitive +)
    (list '- 'primitive -)
    (list '* 'primitive *)
    (list '/ 'primitive /)
    (list '= 'primitive =)
    (list '< 'primitive <)
    (list '<= 'primitive <=)
    (list '> 'primitive >)
    (list '>= 'primitive >=)
    (list 'null? 'primitive null?)
    (list 'pair? 'primitive pair?)
    (list 'list? 'primitive list?)
    (list 'symbol? 'primitive symbol?)
    (list 'car 'primitive car)
    (list 'cdr 'primitive cdr)
    (list 'cons 'primitive cons)
    (list 'list 'primitive list)
    (list 'length 'primitive length)
    (list 'memq 'primitive memq)
    ;        (list 'last 'primitive last)
    (list 'append 'primitive append)
    (list 'boolean? 'primitive boolean?)
    (list 'not 'primitive not)
    (list 'string? 'primitive string?)
    (list 'string-append 'primitive string-append)
    (list 'symbol->string 'primitive symbol->string)
    (list 'string->symbol 'primitive string->symbol)
    (list 'string->number 'primitive string->number)
    (list 'number->string 'primitive number->string)
    (list 'procedure? 'primitive procedure?)
    (list 'eq? 'primitive eq?)
    ;        (list 'neq? 'primitive neq?)
    (list 'equal? 'primitive equal?)
    ; syntax
    ;        (list 'quote 'syntax si-quote)
    ;        (list 'set-car! 'primitive set-car!)
    ;        (list 'set-cdr! 'primitive set-cdr!)

    ; addtional
    (list 'assoc 'primitive assoc)
    ))

; Interpreter body
(define (Interpreter )
  (let loop()
    (display ">>")
    (let ((input (read)))		; read User's input
      (cond 
	((equal? input 'quit) (display "Intepreter end."))	; if input == "quit"
	(else 
	  (display (si-eval input '() )) ; output evaluation of input
	  (newline)
	  (loop))))))

; eval expr in environment
(define (si-eval expr env)
  (cond
    ((self-evaluation? expr) expr)
    ((symbol? expr) (cdr (lookup expr env)))
    ((pair? expr) (si-eval-func expr '()))
    (else (error "Wrong input!"))))

; eval function. expr -> (functionname .args)
(define (si-eval-func expr env)
  (let* ((name (car expr)) (var (lookup name env)) )
    (if (equal? var #f)
      (error "cannot lookup.")
      (let ((type (cadr var)) (func (caddr var)) )
	(cond
	  ((equal? type 'primitive)
	   (si-apply-primitive func (cdr expr) env))
	  ((equal? type 'syntax) '())
	  ((equal? type 'closure) '())
	  (else (error "unknown ")))))))

; apply primitive function
(define (si-apply-primitive func params env)
  (apply func (map (lambda (x) (si-eval x env)) params)))

; if expr is self-evaluation form, return #t
(define (self-evaluation? expr)
  (and (not (pair? expr)) (not (symbol? expr)) ))

; Lookup var from environment and return (var . value).
; If var don't exist reurn #f.
(define (lookup name env)
  (let ((value (assoc name env))) ; lookup in local environment
    (if value
      value
      (assoc name GLOBAL-ENV)))) ; lookup in global environment

; add (var . val) to environment and return new environment
(define (add-var2env vars vals env)
  (cond
    ((null? var) env)
    ((symbol? vars) (cons (cons vars vals ) env))
    (else (cons 
	    (cons (car vars) (car vals))
	    (add-var2env((cdr vars) (cdr vals) env))))))

; The following, this interpreter's treatment of syntax
; quote
(define (si-quote expr env)
  (cadr expr))

(Interpreter)
