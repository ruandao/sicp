
(load "stream.scm")
(load "stream-integers.scm")

(define (mul-stream streamA streamB)
  (stream-map * streamA streamB))

(define factorials (cons-stream 1 (mul-stream (stream-cdr integers)
					      factorials)))


#|


 (stream-head factorials 10)




|#
