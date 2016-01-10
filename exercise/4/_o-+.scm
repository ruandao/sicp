(load "apply-eval.scm")

(define install-o-+
  (lambda()
    (define (plus? exp)
      (tagged-list? exp '+))
    (define (plus-args exp)
      (cdr exp))
    (define (plus args)
      (apply + args))
    (define (eval-plus exp env)
      (plus (map (lambda(var) (eval var env))
		 (plus-args exp))))

    (put-type-query! plus? '(+))
    (put-proc! '(eval +) eval-plus)
    ))
