(load "apply-eval.scm")
(load "utils.scm")

(define install-eq
  (lambda()
    (define (eq-? exp)
      (tagged-list? exp 'eq?))
    (define (make-eq? arg1 arg2)
      (list 'eq? arg1 arg2))
    (define (eq-arg1 exp)
      (cadr exp))
    (define (eq-arg2 exp)
      (caddr exp))
    (define (eval-eq exp env)
;      (newline)
;      (display "eval-eq: ")(newline)
;      (display "arg1: ")(display (eq-arg1 exp))(newline)
;      (display "arg2: ")(display (eq-arg2 exp))(newline)
;      (display (eval (eq-arg1 exp) env))(display " <--eval-arg1: ")(newline)
;      (display (eval (eq-arg2 exp) env))(display " <--eval-arg2: ")(newline)
      (equal? (eval (eq-arg1 exp) env)
	   (eval (eq-arg2 exp) env)))

    (put-type-query! eq-? '(eq?))
    (put-proc! '(eval eq?) eval-eq)
    (put-proc! '(make-eq?) make-eq?)
    ))
