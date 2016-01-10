(load "stream.scm")
(load "stream-integral.scm")

(define (RLC R L C dt)
  (define (x iL0 vC0)
    (define iL (integral (delay diL) iL0 dt))  
    (define vC (integral (delay dVc) vC0 dt))
    (define dVc (scale-stream iL (/ -1 C)))
    (define diL (add-stream (scale-stream iL (/ (- R) L))
			    (scale-stream vC (/ 1 L))))
    (cons vC iL))
  x)


#|

((lambda()
   (define proc (RLC 1 1 0.2 0.1))
   (define p (proc 10 0))
   (display (stream-head 'vC (car p) 5))
   (newline)
   (display (stream-head 'iL (cdr p) 5))
))


;Loading "stream.scm"... done
;Value: list-stream

;Loading "stream-integral.scm"...
;  Loading "stream.scm"... done
;... done
;Value: integral

;Value: rlc

(vc 0 -5. -9.5 -13.3 -16.245)
(il 10 9. 7.6 5.89 3.970999999999999)
;Unspecified return value

|#
