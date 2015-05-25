; macro library for my Scheme Interpreter
; Nakano Mizuki

(define-macro
  and
  (lambda args
    (if (null? args)
      #t
      (if (null? (cdr args))
        (car args)
        `(if ,(car args) (and ,@(cdr args)) #f)))))

(define-macro
  let
  (lambda (args . body)
    `((lambda ,(map car args) ,@body ) ,@(map cadr args))))
