(load "apply-eval.scm")

(define install-do
  (lambda()
    (define (do? exp)
      (tagged-list? exp 'do))
    (define (make-do init-change cmp show)
      (list 'do init-change cmp show))
    (define (do-init-change exp)
      (cadr exp))
    (define (do-init init-change)
      (map (lambda(item) (cons (car item)
			       (cadr item)))
	   init-change))
    (define (do-change init-change)
      (map (lambda(item)
	     (make-set! (car item) (caddr item)))
	   init-change))
    
    (define (do-cmp exp)
      (caddr exp))
    (define (do-show exp)
      (cadddr exp))
    (define (do-inits exp)
      (do-init (do-init-change exp)))
    (define (do-changes exp)
      (do-change (do-init-change exp)))
    
    ))
