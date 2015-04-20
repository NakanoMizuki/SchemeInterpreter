;; Global variable
(define vartable '()) ;Table for Variables


;; Intepreter
(define (interpreter)
  (begin
    (println "Interpreter start.")
    (interpreter-body)
    (println "Interpreter end.")))

;; Interpreter body. Tail loop
(define (interpreter-body)
  (iprint "input> ")
  (let ((sexp (read)))
    (cond
      ((not (list? sexp)) 
       (iprintln "not list"))
      ((not (rightInput? sexp))
       (iprintln "wrong input")
       (interpreter-body))
      (else
       (let ((head (car sexp)))
         (cond
           ((equal? head "quit") '())
           ((equal? head "define") '())
           ((equal? head "load") '())
           (else
            (iprintln (value->string (eval-expr sexp)))
            (interpreter-body))))))))


;; check input whether it is right
(define (rightInput? sexp)
  (cond
    ((null? sexp) #f)
    ((< (length sexp) 1) #f)
    (else #t)))

;; print
(define (print str)
  (display str))
(define (println str)
  (print str)
  (newline))
(define (iprint str)
  (print (string-append " " str)))
(define (iprintln str)
  (println (string-append " " str)))
(define (value->string value)
  (cond
    ((eq? value #t) "#t")
    ((eq? value #f) "#f")
    ((procedure? value) "procedure")
    ((number? value) (number->string value))
    ((symbol? value) (symbol->string value))
    ((string? value) value)))


;; eval expr
(define (eval-expr exp)
  (if (atom? exp)
      exp ;exp = atom
      (let ((func (car exp)) (args (cdr exp))) ;exp != atom
        (cond
          ((equal? func '+) (fold + (cons 0 args )))
          ((equal? func '-) (fold - args))
          ((equal? func '*) (fold * (cons 1 args )))
          ((equal? func '/) (fold / args))
          ((equal? func 'number?) (execUnaryFunc number? args))
          ((equal? func '=) (execBinaryFunc = args))
          ((equal? func '<) (execBinaryFunc < args))
          ((equal? func '<=) (execBinaryFunc <= args))
          ((equal? func '>) (execBinaryFunc > args))
          ((equal? func '>=) (execBinaryFunc >= args))

          ((equal? func 'boolean?) (execUnaryFunc boolean? args))
          ((equal? func 'not) (execUnaryFunc not args))

          ((equal? func 'string?) (execUnaryFunc string? args))

          ((equal? func 'procedure?) (execUnaryFunc procedure? args))

          ((equal? func 'eq?) (execBinaryFunc eq? args))
          ((equal? func 'neq?) (execBinaryFunc neq? args))
          ((equal? func 'equal?) (execBinaryFunc equal? args))

          (else (iprintln "unknown"))))))

;; atom?
(define (atom? exp)
  (cond
    ((pair? exp) #f)
    ((null? exp) #f)
    (else #t)))

;; Macro
;; if num of args is less than needed return error,else exec function(1arg)
(define (execUnaryFunc func args)
  (func (car args)))

;; Macro
;; if num of args is less than needed return error,else exec function(2arg)
(define (execBinaryFunc func args)
  (func (car args) (cadr args)))


;;fold (func:procedure which have 2args)
(define (fold func list)
  (cond
    ((null? list) '())
    ((eq? (length list) 1) (func (car list)))
    (else (func (car list) (fold-loop func (cdr list))))))
(define (fold-loop func list)
  (cond
    ((eq? (length list) 1)(car list))
    (else (func(func (car list) (fold-loop func (cdr list)))))))

;; add variable to vartable
(define (addVariable name type value)
  (let ((newvar (cons name (cons type (cons value '() )))))
    (set! vartable (append vartable (cons newvar '() )))))

;; get variable from vartable
(define (getVariable varname)
  (getFromVartable vartable varname))
(define (getFromVartable currenttable varname)
  (cond
    ((null? currenttable) #f)
    ((equal? (caar currenttable) varname) (car currenttable))
    (else (getFromVariable (cdr currenttable) varname))))


(interpreter) ;Run Interpreter
