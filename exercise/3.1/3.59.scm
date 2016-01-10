
(load "stream.scm")
(load "stream-integers.scm")

(define (integrate-series sa)
  (stream-map / sa integers))

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define (div . args)
  (apply / args))
(define (negative-series sa)
  (stream-map (lambda(x) (* -1 x)) sa))
(define cosine-series (cons-stream 1
				   (negative-series
				    (integrate-series sine-series))))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))


#|
 
 (stream-head 'integrate-series (integrate-series ones) 10)
 (stream-head 'exp-series exp-series 10)
 (stream-head 'sine-series sine-series 10)
 (stream-head 'cosine-series cosine-series 10)

|#

  
