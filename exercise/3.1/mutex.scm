
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (the-mutex 'acquire)));retry
	    ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
	     false)))

(define (semaphore n)
  (define m (make-mutex))
  (define (acquire)
    (m 'acquire)
    (if (> n 0)
	(begin
	  (set! n (- n 1))
	  (m 'release))
	(begin
	  (m 'release)
	  (acquire))))
  (define (release)
    (m 'acquire)
    (set! n (+ n 1))
    (m 'release))
	  
  (define (dispatch m)
    (cond
     ((eq? m 'acquire) (acquire))
     ((eq? m 'release) (release))
     (else (error "Unsupport op!"))))
  dispatch)

(define (semaphore1 n)
  (let ((cell (list false)))
    (define (acquire)
      (if (test-and-set! cell)
	  (acquire)
	  (begin
	    (if (> n 0)
		(begin
		  (set! n (- n 1))
		  (clear! cell))
		(acquire)))))
    (define (release)
      (if (test-and-set! cell)
	  (release)
	  (begin
	    (set! n (+ n 1))
	    (clear! cell))))
    (define (dispatch m)
      (cond
       ((eq? m 'acquire) (acquire))
       ((eq? m 'release) (release))
       (else (error "Unsupport op!"))))
    dispatch))
