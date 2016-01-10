

;;;; Fibnoacci Numbers


(define (fib n)
  ;; Calculate the nth Fibonacci number recursively
  (if (< n 2)
      1                                 ; base case
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 5)
(fib 5)
(fib 10)

#|

 (fib 20) 
|#



(define (factorial n)
  (if (< n 2)
      1
      (* n (factorial (dec n)))))
(define (dec n)
  (- n 1))

(debug)
