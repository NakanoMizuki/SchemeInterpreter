;
; micro.scm : micro Scheme
;
;             Copyright (C) 2009 Makoto Hiroi
;

; 変数束縛
(define (add-binding vars vals env)
  (cond ((null? vars) env)    ; 実引数が多い場合は無視する
        ((symbol? vars)
         (cons (cons vars vals) env))
        (else
         (cons (cons (car vars) (car vals))
               (add-binding (cdr vars) (cdr vals) env)))))

; 変数を環境から探す
(define (lookup var env)
  ; 局所変数から探す
  (let ((value (assoc var env)))
    (if value
        value
      ; 大域変数から探す
      (assoc var *global-environment*))))

;;; syntax

; (quote x)
(define (m-quote expr env) (cadr expr))

; (if test then else)
(define (m-if expr env)
  (if (m-eval (cadr expr) env)
      (m-eval (caddr expr) env)
    (if (null? (cdddr expr))
        '*undef*
      (m-eval (cadddr expr) env))))

; (lambda (args ...) body ...)
(define (m-lambda expr env)
  (list 'closure expr env))

; (define name s-expr)
(define (m-define expr env)
  ; 内部 define は考慮しない
  (set! *global-environment*
        (cons (cons (cadr expr)
                    (m-eval (caddr expr) env))
              *global-environment*))
  ; シンボルを返す
  (cadr expr))


;;; 関数適用

; 関数値 : (tag ...)
; tag
; syntax    : シンタックス形式 (syntax m-xxx)
; primitive : プリミティブ     (primitive #<subr ...>)
; closure   : クロージャ       (closure (lambda (args ...) body ...) env)

; apply
; procedure := 関数値
(define (m-apply procedure actuals)
  (case (car procedure)
    ((primitive)
     (apply (cadr procedure) actuals))
    ((closure)
     (let ((expr (cadr procedure)))
       ; body の評価
       (eval-body (cddr expr)
                  (add-binding (cadr expr) actuals (caddr procedure)))))
    (else
     (error "unknown procedure type -- m-apply" procedure))))

; body の評価
(define (eval-body body env)
  (cond ((null? (cdr body))
         (m-eval (car body) env))   ; 最後の S 式の評価結果を返す
        (else
         (m-eval (car body) env)
         (eval-body (cdr body) env))))


;;; S 式の評価

; 自己評価フォームか
(define (self-evaluation? expr)
  (and (not (pair? expr)) (not (symbol? expr))))

; eval
(define (m-eval expr env)
  (cond ((self-evaluation? expr) expr)
        ((symbol? expr)
         (cdr (lookup expr env)))
        ((pair? expr)
         (let ((procedure (m-eval (car expr) env)))
           (case (car procedure)
             ((syntax) ((cadr procedure) expr env))
             (else
              (m-apply procedure
                       (map (lambda (x) (m-eval x env)) (cdr expr)))))))
        (else
         (error "unknown expression type -- m-eval" expr))))

; 初期化
(define *global-environment*
        (list
          (list 'car   'primitive car)
          (list 'cdr   'primitive cdr)
          (list 'cons  'primitive cons)
          (list 'eq?   'primitive eq?)
          (list 'pair? 'primitive pair?)
          (list 'if     'syntax m-if)
          (list 'quote  'syntax m-quote)
          (list 'lambda 'syntax m-lambda)
          (list 'define 'syntax m-define)
        ))

;;; read-eval-print-loop
(define (repl)
  (let loop ()
    (display "\n>>> ")
    (display (m-eval (read) '()))
    (newline)
    (loop)))

; 実行
(define (main args)
  ; ファイルの読み込み
  (for-each
    (lambda (name)
      (with-input-from-file name
        (lambda ()
          (let loop ()
            (let ((output (m-eval (read) '())))
              (if (not (eof-object? output))
                  (loop)))))))
    (cdr args)))
  ; 実行
  (repl)