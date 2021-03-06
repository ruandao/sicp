(load "stream.scm")

(define (sign-change-detector a b)
  (cond
   ((and (>= a 0) (< b 0)) -1)
   ((and (< a 0) (>= b 0)) 1)
   (else 0)))
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
			(stream-car input-stream))))
(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings-1
  (stream-map sign-change-detector
	      sense-data
	      (stream-cdr sense-data)))
	      
