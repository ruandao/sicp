(load "apply-eval.scm")

(define install-display
  (lambda()
    (define (display? exp)
      (tagged-list? exp 'display))
    (define (make-display content)
      (list 'display content))
    (define (display-arg exp)
      (cadr exp))
    (define (eval-display exp env)
      (display (eval (display-arg exp) env)))

    (put-type-query! display? '(display))
    (put-proc! '(eval display) eval-display)
    (put-proc! '(make-display) make-display)
    ))
