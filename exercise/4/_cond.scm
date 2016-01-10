(load "apply-eval.scm")

(define install-cond
  (lambda()
    (define (make-begin seq)
      ((get-proc '(make-begin)) seq))
    (define (make-if predicate consequence alternative)
      ((get-proc '(make-if)) predicate consequence alternative))
    (define (make-apply f args)
      ((get-proc (list 'make-apply)) f args))

    
    (define (cond? exp)
      (tagged-list? exp 'cond))
    
    (define (cond-clauses exp)
      (cdr exp))
    (define (empty-clauses? clauses)
      (null? clauses))
    (define (first-clause clauses)
      (car clauses))
    (define (rest-clauses clauses)
      (cdr clauses))
    
    (define (clause-predicate clause)
      (car clause))
    (define (clause-actions clause)
      (cdr clause))
    (define (cond-else? clause)
      (tagged-list? clause 'else))

    (define (=>action? clause)
      ;;      (display clause)(newline)
      (tagged-list? (clause-actions clause) '=>))
    (define (=>action clause)
      (cadr (clause-actions clause)))
    (define (=>argument clause)
      (clause-predicate clause))
    (define (=>action-clause->exp clause)
      ;;      (display "clause: ")(display clause)(newline)
      ;;      (display "=>action: ")(display (=>action clause))(newline)
      ;;      (display "=>argument: ")(display (=>argument clause))(newline)
      (make-apply (=>action clause)
		  (list (=>argument clause))))
    (define (clause->exp clause)
      (cond
       ((=>action? clause) (=>action-clause->exp clause))
       (else (sequence->exp (clause-actions clause)))))
    (define (sequence->exp seq)
      (if (null? (cdr seq))
	  (car seq)
	  (make-begin seq)))
    (define (expand-clauses clauses)
      (cond
       ((empty-clauses? clauses) 'false) ; not else clause
       (else
	(let ((1st (first-clause clauses))
	      (rest (rest-clauses clauses)))
	  (if (cond-else? 1st)
	      (if (empty-clauses? rest)
		  (sequence->exp (clause-actions 1st))
		  (error "ELSE clause isn't last -- COND->IF"
			 clauses))
	      (make-if (clause-predicate 1st)
		       (clause->exp 1st)
;		       (sequence->exp (clause-actions 1st))
		       (expand-clauses rest)))))))
    
    (define (cond->if exp)
      (expand-clauses (cond-clauses exp)))
    
    (define (eval-cond exp env)
      (eval (cond->if exp) env))
    
    (put-type-query! cond? '(cond))
    (put-proc! (list 'eval 'cond) eval-cond)
    )
  )
