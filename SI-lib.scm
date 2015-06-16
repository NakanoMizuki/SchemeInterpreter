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

; cxxxr
(define (caaar ls)
  (car (caar ls)))
(define (caadr ls)
  (car (cadr ls)))
(define (cadar ls)
  (car (cdar ls)))
(define (caddr ls)
  (car (cddr ls)))
(define (cdaar ls)
  (cdr (caar ls)))
(define (cdadr ls)
  (cdr (cadr ls)))
(define (cddar ls)
  (cdr (cdar ls)))
(define (cdddr ls)
  (cdr (cddr ls)))

;map
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

; cond
(define-macro
  cond
  (lambda args
    (if (null? args)
      UNDEF
      (if (not (pair? args))
        (error-ret "Syntax-Error!: cond")
        (if (not (pair? (car args)))
          (error-ret "Syntax-Error!: cond")
          (if (equal? (caar args) 'else)
            (if (null? (cdr args))
              `(begin ,@(cdar args))
              (error-ret "Syntax-Error!: cond: else "))
            `(if ,(caar args)
               (begin ,@(cdar args))
               (cond ,@(cdr args)))))))))

; do
(define-macro
  do
  (lambda (binds test . body)
    (cond
      ((not (pair? binds)) (error-ret "Syntax-Error!: do"))
      ((> (length binds) 3) (error-ret "Syntax-Error!: do")) 
      (else 
        (let ((names (my-map car binds))
              (inits (my-map cadr binds))
              (updates (my-map cddr binds)))
          `(letrec ((loop 
                      (lambda ,names
                        (if ,(car test)
                          (begin ,@(cdr test))
                          (begin
                            ,@body
                            (loop ,@(my-map2 (lambda (x y)
                                               (if (null? x) y (car x)))
                                             updates
                                             names)))))))
             (loop ,@inits)))))))

; let
(define-macro 
  let
  (lambda (args . body)
    (cond
      ((null? args) (error-ret "Syntax-Error!: let"))
      ((null? body) (error-ret "Syntax-Error!: let"))
      ((pair? args) `((lambda ,(my-map car args) ,@body) ,@(my-map cadr args)))        ; normal let
      ((pair? body)
       (let* ((vnames (my-map car (car body)))   ; named let
              (vals (my-map cadr (car body))))
         `(letrec ((,args (lambda ,vnames ,@(cdr body))))
            (,args ,@vals))))
      (else (error-ret "Syntax-Error!: let")))))

; let*
(define-macro
  let*
  (lambda (args . body)
    (cond 
      ((not (pair? args)) (error-ret "Syntax-Error!: let*"))
      ((null? (cdr args)) `(let ,args ,@body))
      (else `(let (,(cons (caar args) (cdar args))) (let* ,(cdr args) ,@body))))))

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
