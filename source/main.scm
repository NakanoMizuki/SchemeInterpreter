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
            (if (parsable? sexp)
                (parse sexp)
                (iprintln "syntax-error"))
            (interpreter-body))))))

;; Check whether input is end term
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

;; check sexp format
(define (parsable? sexp)
  (cond
    ((null? sexp) #f)
    ((not(list? sexp)) #f)
    (else #t)))

;; parse
(define (parse sexp)
  (let ((func (car sexp)) (arg (cdr sexp)))
    (cond
      ((equal? func 'define) (display "define"))
      ((equal? func 'lambda) (display "lambda"))
      (else (display "unknown syntax")))))


;; create variable object
(define (createVar name type value)
  (cons name (cons type (cons value '() ))))

;; add variable to vartable
(define (addVariable var)
  (set! vartable (append vartable (cons var '()))))

;; get variable from vartable
(define (getVariable varname)
  (getFromVartable vartable varname))
(define (getFromVartable currenttable varname)
  (cond
    ((null? currenttable) #f)
    ((equal? (caar currenttable) varname) (car currenttable))
    (else (getFromVariable (cdr currenttable) varname))))


(interpreter) ;Run Interpreter