#;(begin
(define (tokenize sexp)
  (begin
    (display sexp)
    (newline)
    (display "car ")
    (display (car sexp))
    (newline)
    (display "caddr ")
    (display (caddr sexp))
    (newline)))
)

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
  (display sexp))


(interpreter) ;Run Interpreter