#lang planet neil/sicp

(define (fixed-point f first-guess)
  (define tolerance 0.000001)
  (define (colse-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (colse-enough? guess next)
          next
          (try next))))
    (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point-average-damping f first-guess)
  (fixed-point (lambda (y) (average y (f y))) first-guess))

(define (fp f first-guess)
  (fixed-point-average-damping f first-guess))

(fp (lambda (y) (+ 1 (/ 1 y))) 1.0)

(define (cont-frac-iter n d m k)
  (define (iter i  value_i_plus_1)
    (cond ((= i 0) value_i_plus_1)
          (else (iter (- i 1) (/ (n i) (+ (d i) (* (m i) value_i_plus_1)))))))
  (iter k 0))

(define (cont-frac-recursive n d m k)
  (define (recursive i)
    (cond ((= i k) (/ (n i) (d i)))
          (else (/ (n i) (+ (d i) (* (m i) (recursive (+ i 1))))))))
  (recursive 1))

(define (d i)
  (define (f i)
    (cond ((= (remainder i 3) 0) (* 2 (+ 1 (/ i 3))))
          (else 1)))
  (cond ((= i 1) 1)
        ((= i 2) 2)
        (else (f (- i 2))
        )))

(define (square x) (* x x))
(define (tan-cf-iter x k)
  (define (n i) 
    (cond ((= i 1) x)
          (else (square x))))
  
  (define (d i) 
    (- (* 2 i) 1))
  
  (define (m i) -1.0)
  (cont-frac-iter n
                  d
                  m
                  k))

(define (tan-cf-recursive x k)
  (define (n i) 
    (cond ((= i 1) x)
          (else (square x))))
  
  (define (d i) 
    (- (* 2 i) 1))
  
  (define (m i) -1.0)
  (cont-frac-recursive n
                  d
                  m
                  k))

(tan-cf-iter 30 100)
(tan-cf-recursive 30 100)
(tan 30)