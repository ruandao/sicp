(load "3.71.scm")

(define (square x) (* x x))
(define (square-sum p)
  (+ (square (car p)) (square (cadr p))))

;; 求连续流
;; 要求,至少连续n 个的权重一致
;; 但是,当大于n 个的权重一致时, 也要能够显示
(define (consecutive-n-stream? weight n v s)
  (cond
   ((stream-null? s) false)
   ((= n 0) true)
   ((= (weight v)
       (weight (stream-car s)))
    (consecutive-n-stream? weight (- n 1) v (stream-cdr s)))
   (else false)))
(define (consecutive-stream-other weight v s)
  (cond
   ((stream-null? s) the-empty-stream)
   ((= (weight v) (weight (stream-car s)))
    (cons-stream (stream-car s)
		 (consecutive-stream-other weight v (stream-cdr s))))
   (else (consecutive-stream-other weight v (stream-cdr s)))))

(define (same-stream-proc cal v s other-proc)
  (cond
   ((stream-null? s) the-empty-stream)
   ((= (cal v) (cal (stream-car s)))
    (cons-stream (stream-car s)
		 (same-stream-proc cal v (stream-cdr s) other-proc)))
   (else (other-proc s))))
(define (consecutive-stream weight n s)
  (cond
   ((stream-null? s) the-empty-stream)
   ((consecutive-n-stream? weight n (stream-car s) s)
    (same-stream-proc weight
		      (stream-car s)
		      s
		      (lambda(s1) (consecutive-stream weight n s1))))
   (else (consecutive-stream weight n (stream-cdr s)))))

#|

((lambda()
   (define ps (weighted-pair square-sum
			     integers
			     integers))
   (define three-different-ways-square-sum
     (consecutive-stream square-sum
			 3
			 ps))
   (stream-head 'tdwss
		three-different-ways-square-sum
		10)
   ))

|#
