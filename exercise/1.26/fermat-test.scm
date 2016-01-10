
(define (expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp)
    (remainder (square (expmod base (/ exp 2) m))
	       m))
   (else
    (remainder (* base (expmod base (- exp 1) m))
	       m))))
(define (fermat-test n)
  (define (fermat-test-m m)
    (cond
     ((= m 0) true)
     (else (and (try-it (+ 1 (random (- n 1))))
		(fermat-test-m (- m 1))))))
  (define (try-it a)
    (= (expmod a n n) a))
  (fermat-test-m 10))

(define prime? fermat-test)

#|

((lambda()
   (define (p n)
     (display "prime: ")
     (display n)
     (newline))

   (define (test-lt n)
     (cond
      ((> n 1)
       (if (prime? n)
	   (p n))
       (test-lt (- n 1)))))
   (test-lt 100)
   ))


|#
