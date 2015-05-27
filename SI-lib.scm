; macro library for my Scheme Interpreter
; Nakano Mizuki


(define UNDEF "#<undefined>")

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
(define (my-map2 fn ls1 ls2)
  (if (null? ls1)
    '()
    (cons (fn (car ls1) (car ls2)) (my-map2 fn (cdr ls1) (cdr ls2)))))

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

; neq?
(define-macro
  neq?
  (lambda (x y)
    (not (eq? x y))))

; begin
(define-macro
  begin
  (lambda args
    (if (null? args)
      `((lambda () UNDEF))
      `((lambda () ,@args)))))

; let
(define-macro 
  let
  (lambda (args . body)
    (if (pair? args)
      `((lambda ,(my-map car args) ,@body) ,@(my-map cadr args))        ; normal let
      (let* ((vnames (my-map car (car body)))   ; named let
             (vals (my-map cadr (car body))))
        `(letrec ((,args (lambda ,vnames ,@(cdr body))))
           (,args ,@vals))))))

; let*
(define-macro
  let*
  (lambda (args . body)
    (if (null? (cdr args))
      `(let ,args ,@body) 
      `(let (,(cons (caar args) (cdar args))) (let* ,(cdr args) ,@body)))))

; letrec
(define-macro
  letrec
  (lambda (args . body)
    (let* ((names (my-map car args))
           (vals (my-map cadr args))
           (let-args (my-map (lambda (x) (list x UNDEF)) names))
           (let-body (my-map2 (lambda (name val) `(set! ,name ,val)) names vals)))
      `(let ,let-args
         ,@(my-map2 (lambda (name val) (list 'set! name val)) names vals)
         ,@body))))
