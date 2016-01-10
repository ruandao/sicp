(load "stream.scm")

(define (integral delayed-integrand init dt)
  (cons-stream init
	       (let ((integrand (force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integranl (delay (stream-cdr integrand))
				(+ (* dt (stream-car integrand))
				   init)
				dt)))))
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-stream (scale-stream dy a)
			  (scale-stream y b)))
  y)

#|



|#
