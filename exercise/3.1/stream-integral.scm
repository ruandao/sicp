(load "stream.scm")

(define (integral delayed-integrand init dt)
  (cons-stream init
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral (delay (stream-cdr integrand))
			       (+ (* (stream-car integrand)
				     dt)
				  init)
			       dt)))))
