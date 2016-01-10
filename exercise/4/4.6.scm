(load "apply-eval.scm")

(define install-let
  (lambda()
    (define (make-lambda args body)
      ((get-proc '(make-lambda)) args body))
    
    (define (let? exp)
      (tagged-list? exp 'let))
    (define (make-let assignments body)
      (list 'let assignments body))
    (define (let-assignments let-clause)
      (cadr let-clause))
    (define (let-body let-clause)
      (caddr let-clause))

    (define (eval-let exp env)
      (let ((new-exp (cons (make-lambda
			    (map car (let-assignments exp))
			    (let-body exp))
			   (map cadr (let-assignments exp)))))
	(eval new-exp env)))

    (put-type-query! let? '(let))
    (put-proc! '(eval let) eval-let)
      
    ))
