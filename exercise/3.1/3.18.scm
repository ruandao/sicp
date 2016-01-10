
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-circle! x)
  (set-cdr! (last-pair x) x))

(define (container-circle? x)
  (define had-used '())
  (define (found? x l)
    (cond
     ((null? l) false)
     ((eq? (car l) x) true)
     (else (found? x (cdr l)))))
  (define (in-had-used? x)
    (found? x had-used))
  (define (add-to-had-used! x)
    (set! had-used (cons x had-used)))

  (define is-circle false)
  (define (set-circle!)
    (set! is-circle true))
  (define (container-circle?-1 x)
    (cond
     (is-circle true)
     ((not (pair? x)) false)
     ((in-had-used? x) true)
     (else
      (begin
	(add-to-had-used! x)
	(or (container-circle?-1 (car x))
	    (container-circle?-1 (cdr x)))))))
  (container-circle?-1 x))

(define l (list 'a 'b 'c))
(container-circle? l)
(make-circle! l)
(container-circle? l)
	 
