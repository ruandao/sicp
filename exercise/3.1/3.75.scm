(load "stream.scm")

(define (sign-change-detector a b)
  (cond
   ((and (>= a 0) (< b 0)) -1)
   ((and (< a 0) (>= b 0)) 1)
   (else 0)))
(define (make-zero-crossings input-stream last-value-1 last-value-2)
  (let ((avg1 (/ (+ last-value-2
		    last-value-1
		    (stream-car input-stream))
		 3))
	(avg2 (/ (+ last-value-1
		    (stream-ref input-stream 0)
		    (stream-ref input-stream 1))
		 3)))
    (cons-stream
     (sign-change-detector avg1 avg2)
     (make-zero-crossings (stream-cdr input-stream)
			  (stream-car input-stream)
			  last-value-1))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings-1
  (stream-map sign-change-detector
	      sense-data
	      (stream-cdr sense-data)))
	      
