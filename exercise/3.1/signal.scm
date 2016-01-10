(load "~/sicp/3.1/agenda.scm")

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
	  (begin (set! signal-value new-value)
		 (call-each action-procedures))
	  'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (call-each procs)
      (if (null? procs)
	  'done
	  (begin
	    ((car procs))
	    (call-each (cdr procs)))))
    (define (dispatch m)
      (cond
       ((eq? m 'get-signal) signal-value)
       ((eq? m 'set-signal!) set-my-signal!)
       ((eq? m 'add-action!) accept-action-procedure!)
       (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (prob name wire)
  (add-action! wire
	       (lambda()
		 (newline)
		 (display name)
		 (display " ")
		 (display (current-time the-agenda))
		 (display "  New-value = ")
		 (display (get-signal wire)))))