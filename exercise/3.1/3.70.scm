(load "stream.scm")
(load "stream-integers.scm")

(define (merge-weighted weight s t)
  (cond
   ((stream-null? s) t)
   ((stream-null? t) s)
   (else (let ((s1 (stream-car s))
	       (t1 (stream-car t)))
	   (cond
	    ((< (weight s1) (weight t1))
	     (cons-stream s1
			  (merge-weighted weight
					  (stream-cdr s)
					  t)))
	    ((> (weight s1) (weight t1))
	     (cons-stream t1
			  (merge-weighted weight
					  s
					  (stream-cdr t))))
	    (else (cons-stream s1
			       (cons-stream t1
					    (merge-weighted
					     weight
					     (stream-cdr s)
					     (stream-cdr t))))))))))

(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
		   (stream-map (lambda(x) (list (stream-car s) x))
			       (stream-cdr t))
		   (weighted-pairs weight
				   (stream-cdr s)
				   (stream-cdr t)))))

#|

((lambda()
   (define (weight-by-sum p)
     (+ (car p) (cadr p)))
   (display (stream-head 'weighted-pair1
			 (weighted-pairs weight-by-sum 
					 integers
					 integers) 10))
   (newline)
   (define (weight-by-poly p)
     (let ((i (car p))
	   (j (cadr p)))
       (+ (* 2 i) (* 3 j) (* 5 i j))))
   (define (filter-2-3-5 x)
     (not (or (= (remainder x 2) 0)
	      (= (remainder x 3) 0)
	      (= (remainder x 5) 0))))
   (define filtered-integers
     ((lambda() (stream-filter filter-2-3-5 integers))))
   (display (stream-head 'weight-by-poly
			 (weighted-pairs weight-by-poly
					 filtered-integers
					 filtered-integers)
			 10))
s1   (newline)
))


|#
