(load "stream.scm")


;;; 参考了下别人的, 新技能get
;;; 把要进行的操作封装到流里面
;;; https://github.com/ivanjovanovic/sicp/blob/master/3.5/e-3.81.scm

(define random-init 100)


(define random-update
  (let ((m 19)
	(a 3)
	(c 5))
    (lambda(x)
      (modulo (+ (* a x) c) m))))

(define random-numbers
  (cons-stream random-init
	       (stream-map random-update random-numbers)))

(define (dp-random-stream opStream)
  (cons-stream random-init
	       (stream-map (lambda(number op)
			     (if (eq? op 'generate)
				 (random-update number)
				 op))
			   (dp-random-stream opStream)
			   opStream)))

(define opStream
  (cons-stream
   'generate
   (cons-stream
    'generate
    (cons-stream
     0
     (cons-stream
      'generate
      (cons-stream
       'generate
       (cons-stream 'generate opStream)))))))

#|
((lambda()
   (define test (dp-random-stream opStream))
   (stream-head 'rand test 30)
   ))
|#
