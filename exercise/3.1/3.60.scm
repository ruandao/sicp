
(load "3.59.scm")

(define (mul-series-1 s1 s2)
  (cons-stream (* (stream-car s1)
		  (stream-car s2))
	       (add-stream (stream-map (lambda(x) (* (stream-car s1)
						     x))
				       (stream-cdr s2))
			   (stream-map (lambda(x) (* (stream-car s2)
						     x))
				       (stream-cdr s1)))))


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
		  (stream-car s2))
	       (add-stream
		(add-stream (scale-stream (stream-cdr s1)
					  (stream-car s2))
			    (scale-stream (stream-cdr s2)
					  (stream-car s1)))
;		(mul-series (stream-cdr s1) (stream-cdr s2)))))
		(cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))
#|

  ((lambda()
   (define s2 (mul-series cosine-series cosine-series))
   (define c2 (mul-series sine-series sine-series))
   (define r (add-stream s2 c2))
   (define (sum items s)
     (cond ((null? items) s)
           (else (sum (cdr items) (+ s (car items))))))
   (sum (cdr (stream-head 'r r 10)) 0.0)

   ;(sum (cdr (stream-head 'ones (mul-series ones ones) 20)) 0) ; ;
   ))


|#
