
(load "stream.scm")

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))



#|

 (stream-head (expand 1 7 10) 20)

 (stream-head (expand 3 8 10) 20)



|#
