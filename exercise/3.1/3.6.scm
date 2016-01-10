
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
     ((= trials-remaining 0)
      (/ trials-passed trials))
     ((experiment)
      (iter (- trials-remaining 1) (+ trials-passed 1)))
     (else
      (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(estimate-pi 100)

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (random-in-x-axis)
    (random-in-range x1 x2))
  (define (random-in-y-axis)
    (random-in-range y1 y2))
  (define (experiment)
    (P (random-in-x-axis) (random-in-y-axis)))
  (monte-carlo trials experiment))

(define (LP x y)
  (<= (+ (square (- x 5))
	 (square (- y 7)))
      (square 3)))

(estimate-integral LP 2 8 4 10 10000)
;Value: 7521/10000
(/ 3.1415926 4)
;Value: .78539815


(define rand
  (let ((xi 100))
    (lambda(op)
      (define (generate)
	(cond
	 ((= xi 0) "Un support range, need reset")
	 (else (begin (set! xi (random xi))
		      xi))))
      (define (reset x)
	(set! xi x)
	xi)
      (cond
       ((eq? op 'generate) (generate))
       ((eq? op 'reset) reset)
       (else "Unsupport operate")))))

(rand 'generate)
((rand 'reset) 100)
