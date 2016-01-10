(load "stream.scm")
(load "stream-integers.scm")

(define (cube x) (* x x x))
(define (cube-sum p)
  (+ (cube (car p)) (cube (cadr p))))
(define (merge-weighted weight s t)
  (cond
   ((stream-null? s) t)
   ((stream-null? t) s)
   (else
    (let ((p1 (stream-car s))
	  (p2 (stream-car t)))
      (cond
       ((<= (weight p1) (weight p2))
	(cons-stream p1
		     (merge-weighted weight
				     (stream-cdr s)
				     t)))
       ((> (weight p1) (weight p2))
	(cons-stream p2
		     (merge-weighted weight
				     (stream-cdr t)
				     s))))))))

(define (weighted-pair weight s t)
  (if (or (stream-null? s) (stream-null? t))
      the-empty-stream
      (cons-stream
       (list (stream-car s) (stream-car t))
       (merge-weighted weight
		       (stream-map (lambda(x)
				     (list (stream-car s) x))
				   (stream-cdr t))
		       (weighted-pair weight
				      (stream-cdr s)
				      (stream-cdr t))))))

(define (consecutive-equal? s)
  (and
   (not (stream-null? s))
   (not (stream-null? (stream-cdr s)))
   (= (stream-car s) (stream-car (stream-cdr s)))))
(define (consecutive-stream s)
  (if (consecutive-equal? s)
      (cons-stream (stream-car s)
		   (consecutive-stream (stream-cdr (stream-cdr s))))
      (consecutive-stream (stream-cdr (stream-cdr s)))))


(define (no-occur filterS t)
  (cond
   ((stream-null? filterS) t)
   ((stream-null? t) the-empty-stream)
   ((not (= (stream-car filterS)
	     (stream-car t)))
    (cons-stream (stream-car t)
		 (no-occur (stream-cdr filterS)
			   (stream-cdr t))))
   (else (no-occur filterS (stream-cdr t)))))



(define ramanujan
  ((lambda()
  (define consecutive-s
    (consecutive-stream
     (stream-map cube-sum
		 (weighted-pair cube-sum
				integers
				integers))))
  (define s (cons-stream (stream-car consecutive-s)
			 (no-occur s
				   (stream-cdr consecutive-s))))
  consecutive-s
  )))


#|

((lambda()
   (stream-head 'ramanujan ramanujan 5)
   ))

;Value 29: (ramanujan 1729 4104 20683 32832 64232)
|#
