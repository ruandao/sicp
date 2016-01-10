
(define (make-mutex)
  (define cell (list false))
  (define (test-and-set! cell)
    (if (car cell)
	true
	(begin (set-car! cell true) false)))
  (define (acquire)
    (if (test-and-set! cell)
	(acquire)))
  (define (release)
    (set-car! cell false))
	
  (define (dispatch m)
    (cond
     ((eq? m 'acquire) (acquire))
     ((eq? m 'release) (release))))
  dispatch)
    
     
    

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
	(mutex 'acquire)
	(let ((val (apply p args)))
	  (mutex 'release)
	  val))
    serialized-p)))
