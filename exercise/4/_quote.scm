(load "apply-eval.scm")

(define install-quote
  (lambda()
    (define (make-cons car cdr)
      ((get-proc '(make-cons)) car cdr))
    (define (make-null)
      ((get-proc '(make-null))))
    (define (self-eval? exp)
      ((get-proc '(self-eval?)) exp))
    
    (define (make-text exp)
      (list 'text exp))
    (define (text? exp)
      (tagged-list? exp 'text))
    (define (text-arg exp)
      (cadr exp))
    (define (eval-text exp env)
      (if (self-eval? (text-arg exp))
	  (text-arg exp)
	  exp))
    (put-type-query! text? '(text))
    (put-proc! '(eval text) eval-text)
    (put-proc! '(make-text) make-text)

    (define (quote? exp)
      (tagged-list? exp 'quote))
    (define (make-quote exp)
      (list 'quote exp))
    (define (quote-arg exp)
      (cadr exp))
    (define (quote-list->list-quote exps)
      (cond
       ((null? exps) (make-null))
       (else
	(make-cons (make-quote (car exps))
		   (quote-list->list-quote (cdr exps))))))
    (define (eval-quote exp env)
      (cond
       ((pair? (quote-arg exp))
	(let ((val (quote-list->list-quote (quote-arg exp))))
;	  (display "quote-list-convert: ")
;	  (display val)(newline)
	  val))
       (else
	(make-text (quote-arg exp)))))

    
    (put-type-query! quote? '(quote))
    (put-proc! '(eval quote) eval-quote)
    (put-proc! '(make-quote) make-quote)
    )
  )
