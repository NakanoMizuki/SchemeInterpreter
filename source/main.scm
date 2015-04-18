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
  (begin
    (iprint "input> ")
    (let ((sexp (read)))
      (if (end? sexp)
          '()
          (begin
            (iprintln (number->string (my-eval sexp)))
            (interpreter-body))))))

;; Check input whether it is end term
(define (end? input)
  (if(equal? input 'quit) #t #f))

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


;; eval s-expr
(define (my-eval sexp)
  (if (atom? sexp) 
      sexp ;sexp = atom
      (let ((func (car sexp)) (args (cdr sexp))) ;sexp != atom
        (cond
          ((equal? func '+) (fold + args))
          ((equal? func '-) (fold - args))
          ((equal? func '*) (fold * args))
          ((equal? func '/) (fold / args))
          ((equal? func 'define) "define")
          ((equal? func 'lambda) "lambda")
          (else "unknown syntax")))))

;; atom?
(define (atom? sexp)
  (cond
    ((pair? sexp) #f)
    ((null? sexp) #f)
    (else #t)))

;;fold (func:procedure which have 2args)
(define (fold func list)
  (if (eq? (length list) 1) 
      (func (car list))
      (func (car list) (fold-loop func (cdr list)))))
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