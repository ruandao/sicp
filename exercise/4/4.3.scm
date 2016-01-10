(load "table.scm")

(define procTable (make-table))
(define (put-proc! tags proc)
  (put-table! tags proc procTable))
(define (get-proc tags)
  (get-table tags procTable))


(define (type-tag exp)
  (car exp))
(define (type-content exp)
  (cdr exp))

(put-proc! 'self-eval
	   (lambda(exp-content env)
	     exp-content))

(define (eval exp env)
  (let ((tag (type-tag exp))
	(content (type-content exp)))
    (let ((proc (get-proc tag)))
      (if proc
	  (proc content env)
	  (error
	   "Unknown type tag -- EVAL" (list tag content))))))


(define (eval exp env)
  (cond
   ((self-evaluating? exp) exp)
   ((variable? exp) (lookup-variable-value exp env))
   ((quoted? exp) (text-of-quotation exp))
   ((application? exp)
    (apply (eval (operator exp) env)
	   (list-of-values (operands exp) env)))
   ((asignment? exp) (eval-assignment exp env))
   ((definition? exp) (eval-definition exp env))
   ((if? exp) (eval-if exp env))
   ((lambda? exp)
    (make-procedure (lambda-parameters exp)
		    (lambda-body exp)
		    env))
   ((begin? exp)
    (eval-sequence (begin-actions exp) env))
   ((cond? exp) (eval (cond->if exp) env))
   (else
    (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arugments)
  (cond
   ((primitive-procedure? procedure)
    (apply-primitive-procedure procedure arguments))
   ((compound-procedure? procedure)
    (eval-sequence
     (procedure-body procedure)
     (extend-environment
      (procedure-parameters procedure)
      arguments
      (procedure-environment procedure))))
   (else
    (error
     "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond
   ((last-exp? exps) (eval (first-exp exps) env))
   (else (eval (first-exp exps) env)
	 (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'ok)

(define (eval-definition exp env)
  (define-varibale! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (self-evaluating? exp)
  (cond
   ((number? exp) true)
   ((string? exp) true)
   (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (asignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)    ; formal parameters
		   (caddr exp)))) ; body
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequence exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond
   ((null? seq) seq)
   ((last-exp? seq) (first-exp seq))
   (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp)
  (cadr exp))
(define (operands exp)
  (cddr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (tagged-list? clause 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))