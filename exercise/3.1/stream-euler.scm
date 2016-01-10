
(load "stream.scm")

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))


(define (make-tableau transform s)
  (cons-stream s
	       (make-tableau transform
			     (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
	      (make-tableau transform s)))

(define (all-kind-accelerate-result s n)
  (display-line (stream-head 'src s n))
  (display-line (stream-head 'euler-transform (euler-transform s) n))
  (display-line (stream-head 'accelerated-sequence
			(accelerated-sequence euler-transform
					      s) n))
  )
#|

 ((lambda()
    (load "stream-pi.scm")
    (stream-ref  (euler-transform pi-stream) 10000)
 ))
 ((lambda()
    (load "stream-pi.scm")
    (stream-head 'pi (accelerated-sequence euler-transform
                                           pi-stream) 100)
 ))


|#
