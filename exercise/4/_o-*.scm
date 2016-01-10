(load "apply-eval.scm")

(define install-o-*
  (lambda()
    (define (mul? exp)
      (tagged-list? exp '*))
    (define (mul-args exp)
      (cdr exp))
    (define (mul args)
      (apply * args))
    (define (eval-mul exp env)
      (mul (map (lambda(var) (eval var env))
		(mul-args exp))))

    (put-type-query! mul? '(*))
    (put-proc! '(eval *) eval-mul)
    ))
