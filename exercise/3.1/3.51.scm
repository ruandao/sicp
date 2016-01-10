(load "~/sicp/3.1/stream.scm")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))
  

(define (show x)
  (display-line x)
  x)

#|

 (define x (stream-map show (stream-enumerate-interval 0 10)))

 (stream-ref x 4)
 (stream-ref x 8)

0
;Value: x

1
2
3
4
;Value: 4

5
6
7
8
;Value: 8

|#
