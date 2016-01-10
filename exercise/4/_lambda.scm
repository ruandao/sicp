(load "apply-eval.scm")


(define install-lambda
  (lambda()
    (define (make-begin exps)
      ((get-proc '(make-begin)) exps))
    
    (define (lambda? exp)
      (tagged-list? exp 'lambda))
    (define (make-lambda parameters body)
      (list 'lambda parameters body))
    (define (lambda-parameters lambda)
      (cadr lambda))
    (define (lambda-body lambda)
      (cddr lambda))
    
    (define (eval-lambda exp env)
      (make-fun exp env))
    
    (put-type-query! lambda? '(lambda))
    (put-proc! '(eval lambda) eval-lambda)
    (put-proc! '(make-lambda) make-lambda)


    (define (fun? exp)
      (and (pair? exp)
	   (tagged-list? (car exp) 'fun)))
    (define (make-fun lambda env)
      (list 'fun lambda env))
    (define (fun-lambda fun)
      (cadr fun))
    (define (fun-body fun)
      (lambda-body (fun-lambda fun)))
    (define (fun-parameters fun)
      (lambda-parameters (fun-lambda fun)))
    (define (fun-env fun)
      (caddr fun))
    
    (define (exp-fun exp)
      (car exp))
    (define (exp-args exp)
      (cdr exp))
    (define (eval-fun exp env)
      (log "eval-fun ")(log exp)(logln)
      (let ((vars (fun-parameters (exp-fun exp))))
	(logln)
	(log "eval-fun:")(logln)
	(log "exp-fun: ")(log (exp-fun exp))(logln)
	(log "    fun-body:")(log (fun-body (exp-fun exp)))(logln)
	(log "exp-args:")(log (exp-args exp))(logln)
	
	(eval (make-begin (fun-body (exp-fun exp)))
	      (add-new-vars-env!
	       vars
	       (map (lambda(var)
		      (logln)
		      (log "(eval var env) vars:")(log vars)(logln)
		      (log "var: ")(log var)(logln)
;		      (log "eval result: ")(log (eval var env))(logln)
		      (eval var env))
		    (exp-args exp))
	       (fun-env (exp-fun exp))))))
    
    (put-type-query! fun? '(fun))
    (put-proc! '(eval fun) eval-fun)
    (put-proc! '(make-fun) make-fun)

   ))

