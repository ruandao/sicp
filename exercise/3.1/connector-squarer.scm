(restart 1)

(load "~/sicp/3.1/connector.scm")
(load "~/sicp/3.1/connector-probe.scm")
(load "~/sicp/3.1/connector-adder.scm")
(load "~/sicp/3.1/connector-multiplier.scm")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
	(if (< (get-value b) 0)
	    (error "square less than 0 -- SQUARER" (get-value b))
	    (set-value! a (sqrt (get-value b)) me))
	(set-value! b (square (get-value a)) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond
     ((eq? request 'I-have-a-value) (process-new-value))
     ((eq? request 'I-lost-my-value) (process-forget-value))
     (else
      (error "Unknown request -- MULTIPLIER" request))))
  (connect a me)
  (connect b me)
  me)

(define a (make-connector))
(define b (make-connector))

(probe 'a a)
(probe 'b b)

(squarer a b)

(set-value! b 9 'user)
