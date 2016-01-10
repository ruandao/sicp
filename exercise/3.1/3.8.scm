




(define f
  (let ((z 0)
	(hadSet false))
    (lambda(n)
      (define w z)
      (cond
       (hadSet w)
       (else
	(begin (set! hadSet true)
	       (set! z n)
	       w))))))


(+ (f 0) (f 1))
(+ (f 0) (f 1))
