
(load "3.60.scm")

(define (invert-unit-series-1 s)
  (define s2 (cons-stream 1 (mul-series
			     (negative-series (stream-cdr s))
			     s2)))

  s2)

#|

((lambda()
   (define m (mul-series exp-series
			 (invert-unit-series exp-series)))
   (sum-list (cdr (stream-head 'x m 50)))
   ))

((lambda()
   (define (reciprocal-series s)
     (cons-stream 1
		  (mul-series (scale-stream (stream-cdr s) -1)
			      (reciprocal-series s))))

   (define foo (reciprocal-series exp-series))
   (define bar (mul-series exp-series foo))
   (stream-head 'exp exp-series 10)
   (stream-head 'foo foo 10)
   (sum-list (cdr (stream-head 'bar bar 100)))
   ))
|#
