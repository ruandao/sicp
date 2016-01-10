

(define (make-accumulator n)
  (let ((sum n))
    (lambda(j)
      (set! sum (+ sum j))
      sum)))

(define A (make-accumulator 5))
(A 10)
(A 10)
