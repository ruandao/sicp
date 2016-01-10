
(load "stream.scm")

(define (sqrt-stream x)
  (define (sqrt-improve guess x)
    (/ (+ guess (/ x guess)) 2))
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda(guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)


#|

 (stream-head '(sqrt 2) (sqrt-stream 2) 10)



|#
