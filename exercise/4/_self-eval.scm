(load "apply-eval.scm")

(define install-self-eval
  (lambda()
    (define (self-eval? exp)
      (or (number? exp)
	  (string? exp)
	  (eq? exp '#t)
	  (eq? exp '#f)
 	  ))

    (define (self-eval exp env)
      exp)
	       
    (put-type-query! self-eval? '(self-eval))
    (put-proc! '(eval self-eval) self-eval)
    (put-proc! '(self-eval?) self-eval?)
    )
  )


