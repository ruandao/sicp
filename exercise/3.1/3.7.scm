
(define (make-account init pwd)

  (define account init)
  (define (withdraw n)
    (if (> account n)
	(begin (set! account (- account n))
	       account)
	"Insufficient funds"))
  (define (deposit n)
    (set! account (+ account n))
    account)
  (define (pwd-error x)
    "Uncorrect password")
  (define (dispatch pwd2 op)
    (cond
     ((not (eq? pwd2 pwd)) pwd-error)
     ((eq? op 'withdraw) withdraw)
     ((eq? op 'deposit) deposit)
     (else "Unsupport operator" op)))
  dispatch)

(define acc (make-account 100 'pwd))
((acc 'jjj 'withdraw) 50)
((acc 'pwd 'withdraw) 60)
((acc 'pwd 'deposit) 10)

(define (make-joint act account-pwd my-pwd)
  (define (error x)
    "Error pwd of joint account")
  (define (dispatch pwd op)
    (cond
     ((eq? pwd my-pwd)
      (act account-pwd op))
     (else error)))
  dispatch)
(define pau-acc
  (make-joint acc 'pwd 'rosebud))

((pau-acc 'rosebud 'withdraw) 50)
((pau-acc 'rosebud 'deposit) 100)
((pau-acc 'rosebud 'withdraw) 40)
((pau-acc 'pwd 'deposit) 100)

