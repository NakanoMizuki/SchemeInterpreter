; Scheme Interpreter
; 2015-04
; Nakano Mizuki


; Interpreter body
(define (Interpreter)
  (display "Interpreter start")
  (newline)
  (let loop()
    (display ">>")
    (let ((input (read)))	; read User's input
      (cond
	((equal? input 'quit)
	 (display "Interpreter End.")
	 (newline))
	(else
	  (let ((output
		  (call/cc (lambda (return) (si-eval input '() return))))) ; if error occured at evaluation,return here
	    (display output)
	    (newline)
	    (loop)))))))

; eval expr in environment
(define (si-eval expr env return)
  (cond
    ((self-evaluation? expr) expr)
    ((symbol? expr) (cdr (lookup expr env return)))
    ((pair? expr) (si-eval-list expr env return))
    (return "Wrong input!")))

; eval function. expr -> (functionname .args)
(define (si-eval-list expr env return)
  (let* ((procedure (si-eval (car expr) env return))
         (type (car procedure)))
    (cond
      ((equal? type 'syntax) ((cadr procedure) expr env return))
      (else 
        (let ((actuals (map (lambda (x) (si-eval x env return)) (cdr expr))))
          (si-apply-procedure procedure actuals return))))))

(define (si-apply-procedure procedure actuals return)
  (cond
    ((equal? (car procedure) 'primitive) (apply (cadr procedure) actuals))
    ((equal? (car procedure) 'closure) (si-apply-closure procedure actuals return))
    (else (return "Error! unknown procedure's type."))))


; apply closure function
(define (si-apply-closure procedure actuals return)
  (let* ((expr (cadr procedure))
         (formals (cadr expr))
         (body (cddr expr))
         (func-env (caddr procedure)))
    (si-eval-body body (add-var2env formals actuals func-env) return)))

; execute all procedure and return last evaluation
(define (si-eval-body body env return)
  (cond
    ((null? (cdr body)) (si-eval (car body) env return))
    (else
      (si-eval (car body) env return)
      (si-eval-body (cdr body) env return))))


; if expr is self-evaluation form, return #t
(define (self-evaluation? expr)
  (and (not (pair? expr)) (not (symbol? expr))))

; Lookup var from environment and return (var . value).
; If var don't exist, set reurn to error statemnt. 
(define (lookup name env return)
  (let ((value (assoc name env))) ; lookup in local environment
    (if value
      value
      (let ((gvalue (assoc name GLOBAL-ENV))) ;lookup in global environment
	(if gvalue
	  gvalue
	  (return (string-append "Error! undefined variable: " (symbol->string name))))))))

; add (var . val) to environment and return new environment
(define (add-var2env vars vals env)
  (cond
    ((null? vars) env)
    ((symbol? vars) (cons (cons vars vals) env))
    (else 
      (cons 
	    (cons (car vars) (car vals))
	    (add-var2env (cdr vars) (cdr vals) env)))))


;;; The following, this interpreter's treatment of syntax
; quote
(define (si-quote expr env return)
  (cadr expr))

; define name expr
(define (si-define expr env return)
  (let ((name (cadr expr))
        (val (si-eval (caddr expr) env return)))
    (set! GLOBAL-ENV (cons (cons name val) GLOBAL-ENV))
    val))

; lambda
(define (si-lambda expr env return)
  (list 'closure expr env))

; if
(define (si-if expr env return)
  (if (si-eval (cadr expr) env return)
    (si-eval (caddr expr) env return)
    (if (null? (cddr expr))
      (return "Error! 'if' need else part.")
      (si-eval (cadddr expr) env return))))

;;; Global environment
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
    (list 'length 'primitive length)
    (list 'memq 'primitive memq)
    ;        (list 'last 'primitive last)
    (list 'append 'primitive append)
    (list 'symbol? 'primitive symbol?)
    (list 'car 'primitive car)
    (list 'cdr 'primitive cdr)
    (list 'cons 'primitive cons)
    (list 'list 'primitive list)
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

    ;        (list 'set-car! 'primitive set-car!)
    ;        (list 'set-cdr! 'primitive set-cdr!)
    
    ; syntax
    (list 'quote 'syntax si-quote)
    (list 'define 'syntax si-define)
    (list 'lambda 'syntax si-lambda)
    ;(list 'set! 'syntax si-set!)
    ;(list 'let 'syntax si-let)
    (list 'if 'syntax si-if)
    ;(list 'cond 'syntax si-cond)
    ;(list 'and 'syntax si-and)
    ;(list 'or 'syntax si-or)
    ;(list 'begin 'syntax si-begin)
    ;(list 'do 'syntax si-do)
    ;(list 'load 'syntax si-load)

    ; addtional
    (list 'assoc 'primitive assoc)
    ))


; run
(Interpreter)
