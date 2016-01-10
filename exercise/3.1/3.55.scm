
(load "stream.scm")
(load "stream-integers.scm")

(define (partial-sums S)
  (define s1 (cons-stream (stream-car S)
			  (add-stream (stream-cdr S)
				      s1)))
  s1)

#|

 (define s (partial-sums integers))
 (stream-head s 15)

|#
