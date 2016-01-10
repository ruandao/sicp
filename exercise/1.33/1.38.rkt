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

(define (cont-frac-iter n d k)
  (define (iter i  value_i_plus_1)
    (cond ((= i 0) value_i_plus_1)
          (else (iter (- i 1) (/ (n i) (+ (d i) value_i_plus_1))))))
  (iter k 0))

(define (cont-frac-recursive n d k)
  (define (recursive i)
    (cond ((= i k) (/ (n i) (d i)))
          (else (/ (n i) (+ (d i) (recursive (+ i 1)))))))
  (recursive 0))
(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                100)
(cont-frac-recursive (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     100)