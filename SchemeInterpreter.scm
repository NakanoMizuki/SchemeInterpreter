; Scheme Interpreter
; 2015-04
; Nakano Mizuki


; Interpreter body
(define (Interpreter)
  (si-load (list 'load "./SI-lib.scm") '() '() (lambda(x)x))
  (si-load (list 'load "./Test.scm") '() '() (lambda(x)x))
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
		  (call/cc (lambda (return) (si-topeval input return (lambda(x)x)))))) ; if error occured at evaluation,return here
	    (display " >> ")
	    (display output)
	    (newline)
	    (loop)))))))

; eval expr at top level
(define (si-topeval expr return cont)
  (cond
    ((and (pair? expr) (equal? (car expr) 'define))
     (si-define expr '() return cont))
    (else (si-eval expr '() return cont))))

; eval expr in environment
(define (si-eval expr env return cont)
  (cond
    ((self-evaluation? expr) (cont expr))
    ((symbol? expr) (cont (cdr (lookup expr env return))))
    ((pair? expr)
     (si-eval (car expr) env return             ; eval first element -> procedure 
              (lambda (procedure) 
                (cond
                  ((not (pair? procedure)) (return "Error! : isn't procedure."))
                  ((equal? (car procedure) 'syntax) ((cadr procedure) expr env return cont))
                  ((equal? (car procedure) 'macro)
                   (si-apply (cdr procedure) (cdr expr) return
                             (lambda (newexpr) (si-eval newexpr env return cont))))
                  (else                                        ; 'closure or 'primitive
                    (si-eval-args (cdr expr) env return
                               (lambda (actuals)
                                 (si-apply procedure actuals return cont))))))))
    (else (return "Error! Wrong input!"))))

; eval args 
(define (si-eval-args args env return cont)
  (if (null? args)
    (cont '())
    (si-eval (car args) env return
             (lambda (x)
               (si-eval-args (cdr args) env return 
                             (lambda (y) (cont  (cons x y))))))))

; apply procedure
(define (si-apply procedure actuals return cont)
  (cond
    ((equal? (car procedure) 'primitive)
     (cond
       ((equal? (cadr procedure) 'call/cc) (si-call/cc (car actuals) return cont))
       ((equal? (cadr procedure) 'continuation) ((caddr procedure) (car actuals)))
       (else (cont (apply (cadr procedure) actuals)))))
    ((equal? (car procedure) 'closure) 
     (let* ((expr (cadr procedure))
            (formals (cadr expr))
            (body (cddr expr))
            (func-env (caddr procedure)))
       (si-eval-body body (add-var2env formals actuals func-env) return cont)))
    (else (return "Error! unknown procedure's type."))))

; execute all procedure and return last evaluation
(define (si-eval-body body env return cont)
  (cond
    ((null? (cdr body)) (si-eval (car body) env return cont))
    ((and (pair? (car body)) (equal? (caar body) 'define))  ;internal define
     (replace-body (car body) (cdr body) env return
                   (lambda (newexpr)
                     (si-eval newexpr env return cont))))
    (else
      (si-eval (car body) env return 
               (lambda (x)
                 (si-eval-body (cdr body) env return cont))))))

; return new body in which internal-define is replaced to let
(define (replace-body defexp restbody env return cont)
  (if (< (length defexp) 3)
    (return "Syntax-Error!: internal-define")
    (cond
      ((pair? (cadr defexp))
       (let* ((name (car (cadr defexp)))
              (args (cdr (cadr defexp)))
              (defbody (cddr defexp))
              (lambda-exp (append (list 'lambda args) defbody))
              (let-args (list (list name lambda-exp))))
         (cont (append (list 'let let-args) restbody))))
      ((not (= 3 (length defexp))) (return "Syntax-Error!: internal-define"))
      (else 
        (let ((name (cadr defexp)))
          (si-eval (caddr defexp) env return
                   (lambda (value)
                     (cont (append (list 'let (list (list name value))) restbody)))))))))

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
(define (si-quote expr env return cont)
  (if (not (= (length expr) 2))
    (return "Syntax-Error!: 'quote'"))
  (cont (cadr expr)))

; define
(define (si-define expr env return cont)
  (if  (< (length expr) 3)
    (return "Syntax-Error: 'define'"))
  (if (pair? (cadr expr))
    (let* ((name (car (cadr expr))) ; (define (procedure arg...) body)
          (args (cdr (cadr expr)))
          (body (cddr expr))
          (li (append (list 'lambda args) body)))
      (si-lambda li env return
                 (lambda (value)
                   (add2GLOBAL-ENV name value)
                   (cont name))))
    (if (not (= (length expr) 3))
      (return "Syntax-Error: 'define'")
      (let ((name (cadr expr)))          ; (define name expr)
        (si-eval (caddr expr) env return
                 (lambda (value)
                   (add2GLOBAL-ENV name value)
                   (cont name)))))))

; lambda
(define (si-lambda expr env return cont)
  (cont (list 'closure expr env)))

; if
(define (si-if expr env return cont)
  (if (or (< (length expr) 2) (> (length expr) 4))
    (return "Syntax Error: 'if'"))
  (si-eval (cadr expr) env return
           (lambda (test)
             (if test
               (si-eval (caddr expr) env return cont)
               (if (null? (cdddr expr))
                 (cont UNDEF)
                 (si-eval (cadddr expr) env return cont))))))

; set
(define (si-set! expr env return cont)
  (if (not (= (length expr) 3))
    (return "Syntax-Error: 'set!'"))
  (let* ((name (cadr expr))
         (cell (lookup name env return)))
    (si-eval (caddr expr) env return
             (lambda(value)
               (set-cdr! cell value)
               (cont (cdr cell))))))

; set-car!
(define (si-set-car! expr env return cont)
  (if (not (= (length expr) 3))
    (return "Syntax Error: 'set-car!'"))
  (let* ((name (cadr expr))
         (cell (lookup name env return)))
    (if (not (pair? (cdr cell)))
      (return (string-append (symbol->string name) "is not pair"))
      (si-eval (caddr expr) env return
               (lambda (value)
                 (set-car! (cdr cell) value)
                 (cont UNDEF))))))

; set-cdr!
(define (si-set-cdr! expr env return cont)
  (if (not (= (length expr) 3))
    (return "Syntax Error: 'set-car!'"))
  (let* ((name (cadr expr))
         (cell (lookup name env return)))
    (if (not (pair? (cdr cell)))
      (return (string-append (symbol->string name) "is not pair"))
      (si-eval (caddr expr) env return
               (lambda (value)
                 (set-cdr! (cdr cell) value)
                 (cont UNDEF))))))

; load
(define (si-load expr env return cont)
  (if (not (= (length expr) 2))
    (return "Syntax Error! : load"))
  (let* ((port (open-input-file (cadr expr))))
    (let loop((newexpr (read port)))
      (if (eof-object? newexpr)
        (close-input-port port)
        (begin
          (si-topeval newexpr return cont)
          (loop (read port))))))
  (cont #t))

; call/cc
(define (si-call/cc procedure return cont)
  (si-apply procedure
            (list (list 'primitive 'continuation cont))
            return cont))

;;; macro
; define macro
(define (si-define-macro expr env return cont)
  (if (pair? (cadr expr))
    (let* ((name (car (cadr expr)))
           (formals (cdr (cadr expr)))
           (body (cddr expr))
           (li (append (list 'lambda formals) body)))
      (si-lambda li env return
                 (lambda (value)
                   (add2GLOBAL-ENV name (cons 'macro value))
                   (cont name))))
    (let ((name (cadr expr)))      ; (define-macro name (lambda () ...))
      (si-eval (caddr expr) env return
               (lambda (value)
                 (add2GLOBAL-ENV name (cons 'macro value))
                 (cont name))))))

; back-quote
(define (si-quasiquote expr env return cont)
  (define (replace ls cont)
    (if (not (pair? ls))
      (cont ls)
      (if (not (pair? (car ls)))
        (replace (cdr ls) 
                 (lambda (x) (cont (cons (car ls) x))))
        (cond
          ((eq? (caar ls) 'unquote)
           (si-eval (cadar ls) env return
                    (lambda (x)                                 ; x is evaled value
                      (replace (cdr ls) 
                               (lambda (y)                      ; y is replaced value
                                 (cont (cons x y)))))))
          ((eq? (caar ls) 'unquote-splicing)
           (si-eval (cadar ls) env return
                    (lambda (x)
                      (replace (cdr ls) 
                               (lambda (y) (cont (append x y)))))))
          (else 
            (replace (car ls)
                     (lambda (x) 
                       (replace (cdr ls)
                                (lambda (y) (cont (cons x y)))))))))))
  (replace (cadr expr) cont))



;;; Global environment
(define UNDEF "#<undefined>")
(define GLOBAL-ENV
  (list
    ; primitive
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

    ; additional primitive
    (list 'call/cc 'primitive 'call/cc)

    ; syntax
    (list 'quote 'syntax si-quote)
    (list 'lambda 'syntax si-lambda)
    (list 'set! 'syntax si-set!)
    (list 'set-car! 'syntax si-set-car!)
    (list 'set-cdr! 'syntax si-set-cdr!)
    (list 'if 'syntax si-if)
    (list 'load 'syntax si-load)


    ; macro
    (list 'quasiquote 'syntax si-quasiquote)
    (list 'define-macro 'syntax si-define-macro)

    ; addtional
    (list 'display 'primitive display)
    (list 'newline 'primitive newline)
    (list 'assoc 'primitive assoc)
    (list 'read 'primitive read)
    (list 'open-input-file 'primitive open-input-file)
    (list 'eof-object? 'primitive eof-object?)
    (list 'close-input-port 'primitive close-input-port)
    ))


; run
(Interpreter)
