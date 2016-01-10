
(define (call-the-cops)
  "U are in caughting")
(define (make-account init pwd)

  (define account init)
  (define error-time 0)
  (define (pwd-error x)
    (set! error-time (+ error-time 1))
    (if (>= error-time 7)
	(call-the-cops)
	(list "Uncorrect password" error-time))
  (define (right-pwd)
    (set! error-time 0))
  
  (define (withdraw n)
    (if (> account n)
	(begin (set! account (- account n))
	       account)
	"Insufficient funds"))
  (define (deposit n)
    (set! account (+ account n))
    account)


  (define (dispatch pwd2 op)
    (cond
     ((not (eq? pwd2 pwd)) pwd-error)
     (else
      (begin (right-pwd)
	     (cond
	      ((eq? op 'withdraw) withdraw)
	      ((eq? op 'deposit) deposit)
	      (else "Unsupport operator" op))))))
  dispatch)

(define acc (make-account 100 'pwd))
((acc 'jjj 'withdraw) 50)


((acc 'pwd 'withdraw) 60)
((acc 'pwd 'deposit) 10)
