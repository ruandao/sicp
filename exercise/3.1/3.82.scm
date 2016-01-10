(load "stream.scm")
(load "3.5.scm")


(define (estimate-integral P x1 x2 y1 y2)
  (define (random-in-x-axis)
    (random-in-range x1 x2))
  (define (random-in-y-axis)
    (random-in-range y1 y2))
  (define (experiment)
    (P (random-in-x-axis) (random-in-y-axis)))
  (define result
    (cons-stream (cons 0 0) ;; (cons passed trials)
		 (stream-map (lambda(pre-result)
			       (if (experiment)
				   (cons (+ (car pre-result) 1)
					 (+ (cdr pre-result) 1))
				   (cons (car pre-result)
					 (+ (cdr pre-result) 1))))
			     result)))
  result)

#|
((lambda()
   (define estimate-pi
     (stream-ref (estimate-integral LP 2 8 4 10) 100000))
   (/ (car estimate-pi) (cdr estimate-pi) 1.0)
   ))
|#
