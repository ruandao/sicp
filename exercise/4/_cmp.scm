(load "apply-eval.scm")

(define install-cmp
  (lambda()
    (define (cmp? exp)
      (or (tagged-list? exp '=)
	  (tagged-list? exp '>)
	  (tagged-list? exp '<)))
    (define (make-cmp op a b)
      (list op a b))
    (define (cmp1 exp)
      (cadr exp))
    (define (cmp2 exp)
      (caddr exp))
    (define (cmpop exp)
      (cond
       ((eq? (car exp) '=) =)
       ((eq? (car exp) '>) >)
       ((eq? (car exp) '<) <)
       (else (error "Unsupport cmp op" exp))))

    (define (eval-cmp exp env)
      ((cmpop exp)
       (eval (cmp1 exp) env)
       (eval (cmp2 exp) env)))

    (put-type-query! cmp? '(cmp))
    (put-proc! '(eval cmp) eval-cmp)
    (put-proc! '(make-cmp) make-cmp)

    ))
