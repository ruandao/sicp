
(load "3.63.scm")


(define (stream-limit s tolerance)
  (let ((a1 (stream-ref s 0))
	(a2 (stream-ref s 1))
	(a3 (stream-ref s 2)))
    (cond
     ((and (< (abs (- a1 a2)) tolerance)
	   (< (abs (- a2 a3)) tolerance)) a2)
     (else (stream-limit (stream-cdr s) tolerance)))))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


#|

((lambda()
   (sqrt 4 0.004)
   (stream-head 'sqrt-stream (sqrt-stream 4) 10)
   ))


|#
