(load "apply-eval.scm")

(define install-begin
  (lambda()
    (define (make-begin exps)
      (cons 'begin exps))
    (define (begin? exp)
      (tagged-list? exp 'begin))
    (define (begin-actions exp)
      (cdr exp))

    (define (last-exp? exps)
      (null? (cdr exps)))
    (define (first-exp exps)
      (car exps))
    (define (rest-exps exps)
      (cdr exps))

    
    (define (eval-sequence exps env)
      (cond
       ((last-exp? exps)
	(log "is last exp: ")(log (first-exp exps))(logln)
	(eval (first-exp exps) env))
       (else
	(log "first-exp: ")(log (first-exp exps))(logln)
	(eval (first-exp exps) env)
	     (eval-sequence (rest-exps exps) env))))
    
    (define (eval-begin exp env)
      (logln)
      (log "eval-begin: ")(logln)
      (log "begin-actions: ")(log (begin-actions exp))(logln)
      (log "env: ")(log env)(logln)
      (eval-sequence (begin-actions exp) env))

    (put-type-query! begin? '(begin))
    (put-proc! (list 'eval 'begin) eval-begin)
    (put-proc! '(make-begin) make-begin)

    )
  )
