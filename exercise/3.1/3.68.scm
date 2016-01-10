(load "stream.scm")
(load "stream-integers.scm")

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs (stream-cdr s) (stream-cdr t))))


#|

((lambda()
(stream-head 'p (pairs integers integers) 10)
))
Does this work? Not work.
Consider what happens if we evaluate (pairs integers integers) using

Louis's definition of pairs: ;Aborting!: maximum recursion depth exceeded



|#
