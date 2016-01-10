(load "~/sicp/3.1/signal.scm")

(define (or-gate a b c)
  (define (logical-or v1 v2)
    (cond
     ((or (= v1 1) (= v2 1)) 1)
     ((and (= v1 0) (= v2 0)) 0)
     (else (error "Invalid signal" v1 v2))))
  (define (proc)
    (let ((new-value (logical-or (get-signal a)
				 (get-signal b))))
      (after-delay or-gate-delay
		   (lambda()
		     (set-signal! c new-value)))))
      
  (add-action! a proc)
  (add-action! b proc)
  'ok)

(define (and-gate a b c)
  (define (logical-and v1 v2)
    (cond
     ((and (= v1 1) (= v2 1)) 1)
     ((or (= v1 1) (= v1 0)
	  (= v2 1) (= v2 0)) 0)
     (else (error "Invalid signal" v1 v2))))
  (define (proc)
    (let ((new-value (logical-and (get-signal a) (get-signal b))))
      (after-delay and-gate-delay
		   (lambda()
		     (set-signal! c new-value)))))
  (add-action! a proc)
  (add-action! b proc)
  'ok)

(define (inverter a b)
  (define (logical-not v)
    (cond
     ((= v 1) 0)
     ((= v 0) 1)
     (else (error "Invalid signal" v))))
  (define (proc)
    (let ((new-value (logical-not (get-signal a))))
      (after-delay inverter-delay
		   (lambda()
		     (set-signal! b new-value)))))
  (add-action! a proc)
  'ok)
