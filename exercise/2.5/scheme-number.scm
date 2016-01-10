
(define (install-scheme-number-package)

  (put 'add '(scheme-number scheme-number) (tag-f 'scheme-number +))
  (put 'sub '(scheme-number scheme-number) (tag-f 'scheme-number -))
  (put 'mul '(scheme-number scheme-number) (tag-f 'scheme-number *))
  (put 'div '(scheme-number scheme-number) (tag-f 'scheme-number /))
  (put 'negative 'scheme-number (tag-f 'scheme-number (lambda(x) (- x))))
  'done)
