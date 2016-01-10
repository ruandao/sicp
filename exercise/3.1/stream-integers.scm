
(load "stream.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-stream integers
					    ones)))
(define (integers-n n)
  (stream-stop (lambda(x) (> x n)) integers))

#|

 (trace apply)
 (trace add-stream)
 (stream-ref integers 8) 


|#
