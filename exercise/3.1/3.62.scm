
(load "3.61.scm")

(define (div-series s1 s2)
  (cond
   ((= (stream-car s2) 0)
    (error "denominator shouldn't begin with zero constant"))
   (else (mul-series s1 (invert-unit-series s2)))))



#|

((lambda()
 (define tanX (div-series sine-series cosine-series))
 (stream-head 'tanX tanX 10)
))

|#
