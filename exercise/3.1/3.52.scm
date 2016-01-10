
(load "3.51.scm")

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
			 seq))

#|


 (stream-ref y 7)
 (display-stream z)
 sum

;Value: 136

10
15
45
55
105
120
190
210
;Value: done

;Value: 210





|#
