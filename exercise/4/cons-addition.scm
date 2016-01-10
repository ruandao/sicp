(load "apply-eval.scm")

(define install-cons-addition
  (lambda()
    (define (make-car exp)
      ((get-proc '(make-car)) exp))
    (define (make-cdr exp)
      ((get-proc '(make-cdr)) exp))
    
    (define (cadr? exp)
      (tagged-list? exp 'cadr))
    (define (cadr-arg exp)
      (cadr exp))
    (define (eval-cadr exp env)
      (eval (make-car (make-cdr (cadr-arg exp)))
	    env))

    (put-type-query! cadr? '(cadr))
    (put-proc! '(eval cadr) eval-cadr)


    (define (caar? exp)
      (tagged-list? exp 'caar))
    (define (caar-arg exp)
      (cadr exp))
    (define (eval-caar exp env)
;      (display "eval-caar: ")(make-car (make-car exp))(newline)
      (eval (make-car (make-car (caar-arg exp)))
	    env))

    (put-type-query! caar? '(caar))
    (put-proc! '(eval caar) eval-caar)
    ))
