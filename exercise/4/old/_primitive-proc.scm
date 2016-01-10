(load "apply-eval.scm")

(define install-primitive-proc
  (lambda()

    (define (plus args)
      (cond
       ((null? args) 0)
       (else (+ (car args)
		(plus (cdr args))))))
    (define (mul args)
      (cond
       ((null? args) 1)
       (else (* (car args)
		(mul (cdr args))))))
    (define (div args)
      (define (div-1 dividend divisor)
	(if (= divisor 0)
	    (error "Division by zero signalled by /." args)
	    (/ dividend divisor)))
      (define (div-2 args)
	(cond
	 ((null? (cdr args)) (car args))
	 (else (div-2 (cons (div-1 (car args) (cadr args))
			    (cddr args))))))
      (cond
       ((null? (cdr args)) (div-1 1 (car args)))
       (else (div-2 args))))
    (define (sub args)
      (define (sub-1 minuend subtrahend)
	(- minuend subtrahend))
      (define (sub-2 args)
	(cond
	 ((null? (cdr args)) (car args))
	 (else (sub-2 (cons (sub-1 (car args) (cadr args))
			    (cddr args))))))
      (cond
       ((null? (cdr args)) (sub-1 0 (car args)))
       (else (sub-2 args))))

    


    (define (primitive-proc? exp)
      (or (tagged-list? exp '+)
	  (tagged-list? exp '*)
	  (tagged-list? exp '/)
	  (tagged-list? exp '-)))

    
    (define (primitive-proc exp)
      (cond
       ((tagged-list? exp '+) plus)
       ((tagged-list? exp '*) mul)
       ((tagged-list? exp '/) div)
       ((tagged-list? exp '-) sub)
       (else (error "Unknown type -- PRIMITIVE-PROC" proc))))
    (define (primitive-args exp)
      (cdr exp))
    (define (apply-primitive-proc proc args)
      ((primitive-proc proc) args))
    (define (eval-primitive-proc exp env)
      ((primitive-proc exp)
       (map (lambda(var) (eval var env))
	    (primitive-args exp))))

    (put-type-query! primitive-proc? '(primitive-proc))
    (put-proc! '(eval primitive-proc) eval-primitive-proc)
    
    ))
