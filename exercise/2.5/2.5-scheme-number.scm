
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (raise scheme-number)
    ((get 'make 'rational) (contents scheme-number) 1))

  (put 'add '(scheme-number scheme-number)       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)      (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)                  (lambda (x)   (= x 0)))
  (put 'high '(scheme-number scheme-number)      (lambda (x y) (> x y)))
  (put 'level 'scheme-number                   1)
  (put 'raise 'scheme-number                     raise)
  ;; interface
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
