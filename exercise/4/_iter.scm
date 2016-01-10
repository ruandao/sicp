(load "apply-eval.scm")

(define install-do
  (lambda()
    (define (do? exp)
      (tagged-list? exp 'do))
    
    ))
