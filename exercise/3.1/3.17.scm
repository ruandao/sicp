(restart 1)



(define (count-pairs x)
  (define used-list '())
  (define (found? x l)
    (cond
     ((null? l) false)
     ((eq? x (car l)) true)
     (else (found? x (cdr l)))))
  
  (define (had-been-count? x)
    (found? x used-list))
  
  (define (add-to-list! x)
    (set! used-list (cons x used-list)))
  
  (define (count-pairs-1 x)
    (if (or (not (pair? x))
	    (had-been-count? x))
	0
	(begin
	  (add-to-list! x)
	  (+ 1
	     (count-pairs-1 (car x))
	     (count-pairs-1 (cdr x))))))

  (count-pairs-1 x))

(define p2 (cons 'c 'd))
(define p1 (cons 'a p2))
(define p (cons p1 p1))
(set-car! p2 p)
(set-cdr! p2 p1)
(count-pairs p)
