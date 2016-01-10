(define "stream.scm")

(define (integral integrand init dt)
  (define int
    (cons-stream init
		 (add-stream (scale-stream integrand dt)
			     int)))
  int)
(define (RC R C dt)
  (lambda(iStream v0)
    (add-stream (scale-stream iStream R)
		(integral (scale-stream iStream (/ 1 C)) v0 dt))))

#|

公式看的不是很清楚v

|#
    
