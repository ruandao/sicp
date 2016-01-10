(load "apply-eval.scm")

(define install-o-div
  (lambda()
    (define (div? exp)
      (tagged-list? exp '/))
    (define (div-args exp)
      (cdr exp))
    (define (div args)
      (apply / args))
    (define (eval-div exp env)
      (div (map (lambda(var) (eval var env))
		(div-args exp))))

    (put-type-query! div? '(/))
    (put-proc! '(eval /) eval-div)
    ))
