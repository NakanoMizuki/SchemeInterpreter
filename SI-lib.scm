; macro library for my Scheme Interpreter
; Nakano Mizuki


; cxxr
(define (caar ls)
  (car (car ls)))
(define (cadr ls)
  (car (cdr ls)))
(define (cdar ls)
  (cdr (car ls)))
(define (cddr ls)
  (cdr (cdr ls)))

; map
(define (my-map fn ls)
  (if (null? ls)
    '()
    (cons (fn (car ls)) (my-map fn (cdr ls)))))

; and
(define-macro
  and
  (lambda args
    (if (null? args)
      #t
      (if (null? (cdr args))
        (car args)
        `(if ,(car args) (and ,@(cdr args)) #f)))))
; or
(define-macro
  or
  (lambda args
    (if (null? args)
      #t
      (if (null? (cdr args))
        (car args)
        `(let ((+value+ ,(car args)))
           (if +value+ 
             +value+
             (or ,@(cdr args))))))))

; let
(define-macro 
  let
  (lambda (args . body)
    `((lambda ,(my-map car args) ,@body) ,@(my-map cadr args))))

; let*
(define-macro
  let*
  (lambda (args . body)
    (if (null? (cdr args))
      `(let ,args ,@body) 
      `(let (,(cons (caar args) (cdar args))) (let* ,(cdr args) ,@body)))))
