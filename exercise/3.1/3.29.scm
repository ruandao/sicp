(restart 1)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda()
		     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (inverter input output)
  (define (inverter-action-procedure)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda()
		     (set-signal! output new-value)))))
  (add-action! input inverter-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define o1 (make-wire))
  (and-gate a1 a2 o1)
  (inverter o1 output)
  'ok)
