#lang planet neil/sicp

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (colse-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (colse-enough? guess next)
          next
          (try next))))
    (try first-guess))

(define (average a b)
  (/ (+ a b) 2))
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (fixed-point-average-damping f first-guess)
  (fixed-point (lambda (y) (average y (f y))) first-guess))

(define (sqrt2 x)
  (fixed-point-average-damping (lambda (y) (/ x y)) 1.0))
(sqrt2 4)

(define (ø x)
  (fixed-point-average-damping (lambda (y) (+ 1 (/ 1 y))) 1.0))
(ø 3)
(ø 4)
(ø 5)