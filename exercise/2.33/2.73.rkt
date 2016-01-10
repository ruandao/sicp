;#lang planet neil/sicp 
#lang racket
(require (planet soegaard/sicp:2:1/sicp))
(define wave einstein)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; sum
(define (install-sum-derivative-package)
  ;; internal procedures
  (define (addend operands) (car operands))
  (define (augend operands) (cdr operands))
  (define (deriv-sum operands var)
    ('+ (deriv (addend operands) var)
              (deriv (augend operands) var)))
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  'done)

;; product
(define (install-product-derivative-package)
  ;; internal procedure
  (define (multiplier operands) (car operands))
  (define (multiplicand operands) (cdr operands))
  (define (deriv-product operands var)
    ('+ ('* (deriv (multiplier operands) var)
            (multiplicand operands))
        ('* (multiplier operands)
            (deriv (multiplicand operands) var))))
  ;; interface to the rest of the system
  (put 'deriv '* deriv-product)
  'done)

(define (install-subtract-derivative-package)
  ;; internal procedure
  (define (subtrahend operands) (car operands))
  (define (minuend operands) (cadr operands))
  (define (deriv-sub operands var)
    ('- (deriv (subtrahend operands) var)
        (deriv (minuend operands) var)))
  ;; interface to the rest of the system
  (put 'deriv '- deriv-sub)
  'done)
(define (install-exponentiation-derivative-package)
  ;; internal procedure
  (define (base operands) (car operands))
  (define (exponent operands) (cadr operands))
  (define (deriv-exponentiation operands var)
    (let ((n (exponent operands))
          (u (base operands)))
      ('* ('* n
              ('** u ('- n 1)))
          (deriv u var))))
  ;; interface to the rest of the system
  (put 'deriv '** deriv-exponentiation)
  'done)