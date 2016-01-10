(load "apply-eval.scm")
(load "_self-eval.scm")

(define install-and-or
  (lambda()
    (define (first-predicate exp)
      (cadr exp))
    (define (rest-predicates exp)
      (cdr exp))
    (define (empty-predicate? exp)
      (null? (cdr exp)))
    (define (last-predicate? exp)
      (empty-predicate? (rest-predicates exp)))
    
    ((lambda()
       (define (and? exp)
	 (tagged-list? exp 'and))

       (define (eval-and exp env)
	 (cond
	  ((last-predicate? exp)
	   (true? (eval (first-predicate exp) env)))
	  (else (and (true? (eval (first-predicate exp) env))
		     (eval-and (rest-predicates exp) env)))))

       (put-type-query! and? '(and))
       (put-proc! (list 'eval 'and) eval-and)
       
       ))

    
    ((lambda()
       (define (or? exp)
	 (tagged-list? exp 'or))
       (define (eval-or exp env)
	 (cond
	  ((last-predicate? exp)
	   (true? (eval (first-predicate exp) env)))
	  (else (or (true? (eval (first-predicate exp) env))
		    (eval-or (rest-predicates exp) env)))))
       (put-type-query! or? '(or))
       (put-proc! (list 'eval 'or) eval-or)
       ))
    
    )
  )

