(load "apply-eval.scm")

(define install-let
  (lambda()
    (define (make-lambda args body)
      ((get-proc '(make-lambda)) args body))
    (define (make-define var val)
      ((get-proc '(make-define)) var val))
    (define (make-begin actions)
      ((get-proc '(make-begin)) actions))
    (define (make-apply f args)
      ((get-proc '(make-apply)) f args))
    
    (define (let? exp)
      (tagged-list? exp 'let))
    (define (make-let assignments body)
      (list 'let assignments body))
    
    (define (let-named? exp)
      (and (let? exp)
	   (= (length exp) 4)))
    (define (let-name exp)
      (cadr exp))
    (define (make-let-named name assignments body)
      (list 'let name assignments body))
    
    (define (let-assignments let-clause)
      (if (let-named? let-clause)
	  (caddr let-clause)
	  (cadr let-clause)))
    (define (let-body let-clause)
      (if (let-named? let-clause)
	  (cadddr let-clause)
	  (caddr let-clause)))

    
    (define (let->combination exp)
      (if (let-named? exp)
	  (make-begin
	   (cons
	    (make-define
	     (let-name exp)
	     (make-lambda (map car (let-assignments exp))
			  (let-body exp)))
	    (list
	     (make-apply
	      (let-name exp)
	      (map cadr (let-assignments exp))))))
	  (cons (make-lambda (map car (let-assignments exp))
			     (let-body exp))
		(map cadr (let-assignments exp)))))
    (define (eval-let exp env)
      (log (let->combination exp))(log "let->combination")(logln)
      (eval (let->combination exp) env))

    (put-type-query! let? '(let))
    (put-proc! '(eval let) eval-let)
    (put-proc! '(make-let) make-let)
    ))

(define install-let*
  (lambda()
    (install-let)
    
    (define (make-let args body)
      ((get-proc '(make-let)) args body))

    (define (let*? exp)
      (tagged-list? exp 'let*))
    (define (make-let* assignments body)
      (list 'let* assignments body))
    (define (let*-assignments let-clause)
      (cadr let-clause))
    (define (let*-body let-clause)
      (caddr let-clause))

    (define (expand-let* assignments body)
      (cond
       ((null? (cdr assignments))
	(make-let assignments body))
       (else
	(make-let (list (car assignments))
		  (expand-let* (cdr assignments) body)))))
    (define (let*->nested-lets exp)
      (expand-let* (let*-assignments exp)
		   (let*-body exp)))
    (define (eval-let* exp env)
      (eval (let*->nested-lets exp) env))

    (put-type-query! let*? '(let*))
    (put-proc! '(eval let*) eval-let*)
    (put-proc! '(make-let*) make-let*)
    ))
