
(define (memo-proc proc)
  (let ((already-run? false)
	(result false))
    (lambda()
      (if (not already-run?)
	  (begin
	    (set! already-run? true)
	    (set! result (proc))
	    result)
	  result))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map-1 proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (let ((s1 (stream-car s)))
	(begin (proc s1)
	       (stream-for-each proc (stream-cdr s))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (stream-stop proc stream)
  (cond
   ((stream-null? stream) the-empty-stream)
   ((proc (stream-car stream)) the-empty-stream)
   (else (cons-stream (stream-car stream)
		      (stream-stop proc (stream-cdr stream))))))

(define (stream-filter pred stream)
  (cond
   ((stream-null? stream) the-empty-stream)
   ((pred (stream-car stream))
    (cons-stream (stream-car stream)
		 (stream-filter pred
				(stream-cdr stream))))
   (else (stream-filter pred (stream-cdr stream)))))

(define (display-line x)
  (newline)
  (display x))
(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      'done
      (cons-stream (apply proc (map stream-car streams))
		   (apply stream-map (cons proc (map stream-cdr streams))))))
(define (add-stream a b)
  (stream-map + a b))

(define (scale-stream s scale)
  (stream-map (lambda(x) (* x scale)) s))

(define (stream-head tag s n )
  (define (stream-head-1 s n)
    (if (> n 0)
	(cons (stream-car s)
	      (stream-head-1 (stream-cdr s) (- n 1)))
	'()))
  (cons tag (stream-head-1 s n)))

(define (sum-list s)
  (define (sum-list-1 s r)
    (cond
     ((null? s) r)
     (else (sum-list-1 (cdr s) (+ (car s) r)))))
  (sum-list-1 s 0.0))

(define (display-list items)
  (cond
   ((null? items) 'done)
   (else (begin (display-line (car items))
		(display-list (cdr items))))))

(define (list-stream items)
  (cond
   ((null? items) the-empty-stream)
   (else (cons-stream (car items)
		      (list-stream (cdr items))))))
