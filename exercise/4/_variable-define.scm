(load "apply-eval.scm")

(define install-variable-define
  (lambda()

   ;; 变量
   (define (variable? exp)
     (symbol? exp))

   (define (lookup-variable-value exp env)
     (let ((val (lookup-var-env exp env)))
;       (display "var: ")(display exp)(newline)
;       (display "val: ")(display val)(newline)
       val))
   (define (make-variable var val env)
     (add-new-var-env! var val env))

   
   (put-type-query! variable? '(variable))
   (put-proc! (list 'eval 'variable) lookup-variable-value)
   ;(put-proc! '(make-variable) make-variable)


   ;; 定义
   (define (make-lambda vars body)
     ((get-proc '(make-lambda)) vars body))

   (define (define? exp)
     (tagged-list? exp 'define))
   (define (make-define var val)
     (list 'define var val))
   (define (define-var exp)
     (if (symbol? (cadr exp))
	 (cadr exp) ; (define xxx  ddd)
	 (caadr exp))) ; (define (xxx ddd) ...)

   (define (define-val exp)
     (if (symbol? (cadr exp))
	 (caddr exp)
	 (make-lambda (cdadr exp)    ; formal parameters
		      (caddr exp)))) ; body
   (define (define-var! symbol value env)
     (make-variable symbol value env))
   
   (define (eval-define exp env)
     (define-var! (define-var exp)
       (eval (define-val exp) env)
       env)
     'ok)
   
   (put-type-query! define? '(define))
   (put-proc! (list 'eval 'define)
	      eval-define)


   ;; 赋值
   (define (assignment? exp)
     (tagged-list? exp 'set!))
   
   (define (eval-assignment exp env)
     (update-var-env! (assignment-var exp)
		      (eval (assignment-val exp) env)
		      env)
     'ok)
   (define (assignment-var exp)
     (cadr exp))
   (define (assignment-val exp)
     (caddr exp))


   (put-type-query! assignment? '(assignment))
   (put-proc! (list 'eval 'assignment)
	      eval-assignment)
   (put-proc! '(make-define) make-define)

   ))
