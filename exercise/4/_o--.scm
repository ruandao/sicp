(load "apply-eval.scm")

(define install-o--
  (lambda()
    (define (sub? exp)
      (tagged-list? exp '-))
    (define (sub-args exp)
      (cdr exp))
    (define (sub args)
      (apply - args))
    (define (eval-sub exp env)
      (sub (map (lambda(var) (eval var env))
		(sub-args exp))))

    (put-type-query! sub? '(-))
    (put-proc! '(eval -) eval-sub)
    ))
