; Scheme Interpreter
; 2015-04
; Nakano Mizuki


; Interpreter body
(define (Interpreter)
  (si-load (list 'load "./SI-lib.scm") '() '())
  (display "Interpreter start")
  (newline)
  (let loop()
    (let ((input (read)))	; read User's input
      (cond
	((equal? input 'quit)
	 (display "Interpreter End.")
	 (newline))
	(else
	  (let ((output
		  (call/cc (lambda (return) (si-eval input '() return))))) ; if error occured at evaluation,return here
	    (display " >> ")
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
      ((equal? type 'macro) (si-eval (si-apply-procedure (cdr procedure) (cdr expr) return) env return))
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

; add (name . value) to GLOBAL-ENVIRONMENT
(define (add2GLOBAL-ENV name value)
  (set! GLOBAL-ENV (cons (cons name value)
                         GLOBAL-ENV)))

;;; The following, this interpreter's treatment of syntax
; quote
(define (si-quote expr env return)
  (if (not (= (length expr) 2))
    (return "Syntax-Error!: 'quote'"))
  (cadr expr))

; define
(define (si-define expr env return)
  (if  (< (length expr) 3)
    (return "Syntax-Error: 'define'"))
  (if (pair? (cadr expr))
    (let* ((name (car (cadr expr))) ; (define (procedure arg...) body)
          (args (cdr (cadr expr)))
          (body (cddr expr))
          (li (append (list 'lambda args) body)))
      (add2GLOBAL-ENV name (si-lambda li env return))
      name)
    (if (not (= (length expr) 3))
      (return "Syntax-Error: 'define'")
      (let ((name (cadr expr))          ; (define name expr)
            (val (si-eval (caddr expr) env return)))
        (add2GLOBAL-ENV name val)
        name))))

; lambda
(define (si-lambda expr env return)
  (list 'closure expr env))

; if
(define (si-if expr env return)
  (if (or (< (length expr) 2) (> (length expr) 4))
    (return "Syntax Error: 'if'"))
  (if (si-eval (cadr expr) env return)
    (si-eval (caddr expr) env return)
    (if (null? (cdddr expr))
      UNDEF
      (si-eval (cadddr expr) env return))))

; set
(define (si-set! expr env return)
  (if (not (= (length expr) 3))
    (return "Syntax-Error: 'set!'"))
  (let* ((name (cadr expr))
        (value (si-eval (caddr expr) env return))
        (cell (lookup name env return)))
    (set-cdr! cell value)
    (cdr cell)))

; set-car!
(define (si-set-car! expr env return)
  (if (not (= (length expr) 3))
    (return "Syntax Error: 'set-car!'"))
  (let* ((name (cadr expr))
         (value (si-eval (caddr expr) env return))
         (cell (lookup name env return)))
    (if (not (pair? (cdr cell)))
      (return (string-append (symbol->string name) "is not pair")))
    (set-car! (cdr cell) value)
    UNDEF))

; set-cdr!
(define (si-set-cdr! expr env return)
  (if (not (= (length expr) 3))
    (return "Syntax Error: 'set-car!'"))
  (let* ((name (cadr expr))
         (value (si-eval (caddr expr) env return))
         (cell (lookup name env return)))
    (if (not (pair? (cdr cell)))
      (return (string-append (symbol->string name) "is not pair")))
    (set-cdr! (cdr cell) value)
    UNDEF))

; load
(define (si-load expr env return)
  (if (not (= (length expr) 2))
    (return "Syntax Error! : load"))
  (let* ((file (cadr expr))
        (port (open-input-file file)))
    (let loop((newexpr (read port)))
      (if (eof-object? newexpr)
        (close-input-port port)
        (begin
          (si-eval newexpr env return)
          (loop (read port))))))
  #t)

;;; macro
; define macro
(define (si-define-macro expr env return)
  (if (pair? (cadr expr))
    (let* ((name (car (cadr expr)))
          (formals (cdr (cadr expr)))
          (body (cddr expr))
          (li (append (list 'lambda formals) body))
          (value (si-lambda li env return)))
      (add2GLOBAL-ENV name (cons 'macro value))
      name)
    (let ((name (cadr expr))      ; (define-macro name (lambda () ...))
          (value (si-eval (caddr expr) env return)))
      (add2GLOBAL-ENV name (cons 'macro value))
      name)))

; back-quote
(define (si-quasiquote expr env return)
  (define (replace ls)
    (if (not (pair? ls))
      ls
      (if (not (pair? (car ls)))
        (cons (car ls) (replace (cdr ls)))
        (cond
          ((eq? (caar ls) 'unquote) (cons
                                      (si-eval (cadar ls) env return)
                                      (replace (cdr ls))))
          ((eq? (caar ls) 'unquote-splicing) (append
                                               (si-eval (cadar ls) env return)
                                               (replace (cdr ls))))
          (else (cons (replace (car ls)) (replace (cdr ls))))))))
  (replace (cadr expr)))



;;; Global environment
(define UNDEF "#<undefined>")
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

    ; syntax
    (list 'quote 'syntax si-quote)
    (list 'define 'syntax si-define)
    (list 'lambda 'syntax si-lambda)
    (list 'set! 'syntax si-set!)
    (list 'set-car! 'syntax si-set-car!)
    (list 'set-cdr! 'syntax si-set-cdr!)
    ;(list 'let 'syntax si-let)
    (list 'if 'syntax si-if)
    ;(list 'cond 'syntax si-cond)
    ;(list 'and 'syntax si-and)
    ;(list 'or 'syntax si-or)
    ;(list 'begin 'syntax si-begin)
    ;(list 'do 'syntax si-do)
    (list 'load 'syntax si-load)

    ; macro
    (list 'quasiquote 'syntax si-quasiquote)
    (list 'define-macro 'syntax si-define-macro)

    ; addtional
    (list 'display 'primitive display)
    (list 'newline 'primitive newline)
    (list 'assoc 'primitive assoc)
    (list 'map 'primitive map)
    ))


; run
(Interpreter)
