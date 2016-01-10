(restart 1)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-circle! x)
  (set-cdr! (last-pair x) x))

(define (container-circle? x)
  (define i1 x)
  (define i2 x)
  (define (i1-step1!)
    (set! i1 (cdr i1))
    i1)
  (define (i2-step2!)
    (set! i2 (cdr i2))
    (cond
     ((null? i2) '())
     (else
      (begin
	(set! i2 (cdr i2))
	i2))))
  (define (container-circle?-1)
    (cond
     ((or (null? i1) (null? i2)) false)
     ((eq? i1 i2) true)
     (else
      (begin (i1-step1!)
	     (i2-step2!)
	     (container-circle?-1)))))
  (begin
    (i1-step1!)
    (i2-step2!)
  (container-circle?-1)))

(define l (list 'a 'b 'c 'd))
(display l)
(container-circle? l)
(make-circle! l)
(container-circle? l)
	 
