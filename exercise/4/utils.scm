(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))
      
(define (equal? a b)
  (cond
   ((and (atom? a)
	 (atom? b)) (eq? a b))
   ((or (atom? a)
	(atom? b)) false)
   (else
    (cond
     ((and (null? a) (null? b)) true)
     ((or (null? a) (null? b)) false)
     (else (and (equal? (car a) (car b))
		(equal? (cdr a) (cdr b))))))))


