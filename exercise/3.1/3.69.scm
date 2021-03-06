(load "stream.scm")

(define (triples S T U)
  (if (or (stream-null? S)
	  (stream-null? T)
	  (stream-null? U))
      the-empty-stream
      (let ((i (stream-car S))
	    (j (stream-car T))
	    (k (stream-car U)))
	(if (and (<= i j k)
		 (= (+ (square i) (square j))
		    (square k)))
	    (cons-stream (i, j, k)
			 (triples (stream-cdr S)
				  (stream-cdr T)
				  (stream-cdr U)))
	    (triples (stream-cdr S)
		     (stream-cdr T)
		     (stream-cdr U)))))
