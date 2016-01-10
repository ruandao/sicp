(load "apply-eval.scm")

(define install-if
  (lambda()

    (define (if? exp)
      (tagged-list? exp 'if))
    (define (if-predicate exp)
      (cadr exp))
    (define (if-consequence exp)
      (caddr exp))
    (define (if-alternative exp)
      (if (not (null? cdddr))
	  (cadddr exp)
	  'false))
    
    (define (make-if predicate consequent alternative)
      (list 'if predicate consequent alternative))
    
    (define (eval-if exp env)
;      (newline)
;      (display (eval (if-predicate exp) env))(display " <--eval-if ")(newline)
      (if (true? (eval (if-predicate exp) env))
	  (eval (if-consequence exp) env)
	  (eval (if-alternative exp) env)))
    
    (put-type-query! if? '(if))
    (put-proc! (list 'eval 'if) eval-if)
    (put-proc! '(make-if) make-if)


    )
  )
