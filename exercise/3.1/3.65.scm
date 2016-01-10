
(load "stream.scm")
(load "stream-euler.scm")

(define (negative-series s)
  (cons-stream (- (stream-car s))
	       (negative-series (stream-cdr s))))
(define (ln2 n)
  (cons-stream (/ 1.0 n)
	       (negative-series (ln2 (+ n 1)))))


#|

 (stream-head 'ln2 (ln2 1) 10)
 (all-kind-accelerate-result (ln2 1) 10)
|#
