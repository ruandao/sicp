;#lang planet neil/sicp 
#lang racket
(require (planet soegaard/sicp:2:1/sicp))
(define wave einstein)

(define (put x) x)
(define (get x) x)
(define (get-coercion x) x)
(define (put-coercion x) x)
(define (square x) (* x x))
;; type-tag, contents, attach-tag
(define (attach-tag tag contents)
  (if (number? contents)
      contents
      (cons tag contents)))

(define (type-tag value)
  (cond ((number? value) 'scheme-number)
        ((and (pair? value)
              (symbol? (car value)))
         (car value))
        (else (error "Unknown tag -- TYPE-TAG" value))))

(define (contents value)
  (cond ((number? value) value)
        ((and (pair? value)
              (symbol? (car value)))
         (cdr value))
        (else (error "Unknown contents -- CONTENTS" value))))

(define (same-type? t1 t2) (eq? t1 t2))

(define (same-types? types)
  (define (same-types-1? types type)
    (cond
      ((null? types) true)
      ((eq? (car types) type) (same-types-1? (cdr types) type))
      (else false)))
  (cond
    ((> (length types) 1)
     (same-types-1? types (car types)))
    (else false)))


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
  (put 'level 'scheme-number                   1)
  (put 'raise 'scheme-number                     raise)
  ;; interface
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)


(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (raise rational)
    (let ((x (contents rational)))
      ((get 'make-from-real-imag 'complex) (/ (numer x) (denom x)) 0)))

  (define (project rational-number)
    (let ((x (contents rational-number)))
      ((get 'make 'scheme-number) (/ (numer x) (denom x)))))
  
  (define (tag x)
    (attach-tag 'rational x))
  ;; interface
  (put 'add '(rational rational)     (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)     (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)     (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)     (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)    (lambda (x y) (equ? x y)))
  (put '=zero? '(rational)           (lambda (x) (=zero? x)))
  (put 'make 'rational       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(scheme-number) raise)
  (put 'project  '(rational) project)
  (put 'level 'rational            (lambda () (+ (get 'level 'scheme-number) 1)))
  'done)


(define (install-rectangle-package)
  ;; internal
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
    
  ;; interface
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle     '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (install-complex-package)
  ;; import
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (= (real-part z) 0))
  (define (project complex)
    (let ((x (contents complex)))
      ((get 'make 'rational) (real-part x) 1)))
  ;; interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add       '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub       '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul       '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div       '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ?      '(complex complex) (lambda (z1 z2) (equ? z1 z2)))
  (put '=zero?    '(complex)         (lambda (z)     (=zero? z)))
  (put 'real-part '(complex)         real-part)
  (put 'imag-part '(complex)         imag-part)
  (put 'magnitude '(complex)         magnitude)
  (put 'angle     '(complex)         angle)
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang   'complex (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'level 'complex            (lambda () (+ (get 'level 'real) 1)))
  (put 'project 'complex                project)
  'done)


(define (make-rational n d)                 ((get 'make 'rational) n d))
(define (make-scheme-number n)              ((get 'make 'scheme-number) n))
(define (make-complex-from-real-imag x y)   ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)     ((get 'make-from-mag-ang 'complex) r a))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y)(apply-generic 'equ? x y))
(define (=zero? x)(apply-generic '=zero? x))
(define (project x) (apply-generic 'project x))
(define (project? x) (get 'project (type-tag x)))

(define (drop x)
  (if (project? x)
      (if (equ? (raise (project x)) x)
          (drop x)
          x)
      x))

(define (height? x y)
  (> (get 'level x) (get 'level y)))
(define (heightest-type types)
  (define (heightest-type-1 types type)
    (cond
      ((null? types) type)
      ((height? type (car types)) (heightest-type-1 (cdr types) type))
      (else (heightest-type-1 (cdr types) (car types)))))
  (heightest-type-1 types (car types)))
(define (raise-to-type arg type)
  (cond
    ((same-types? (cons (type-tag arg) type)) arg)
    (else (raise-to-type (raise arg) type))))
(define (raise-to-same-type args)
  (let ((htype (heightest-type (map type-tag args))))
    (map (lambda (x) (raise-to-type x htype)) args)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (>= (length args) 2)
              (if (not (same-types? type-tags))
                  (let ((same-type-args (raise-to-same-type (map drop args))))
                    (if same-type-args
                        (apply apply-generic (cons op args))
                        (error "No method for these types"
                               (list op type-tags))))
                  (error "No method for these types"
                         (list op type-tags)))
              (error "No method for these types"
                     (list op type-tags)))))))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (add-terms L1 L2)
  (cond
    ((empty-termlist? L1) L2)
    ((empty-termlist? L2) L1)
    (else
     (let ((t1 (first-term L1)) (t2 (first-term L2)))
       (cond
         ((> (order t1) (order t2))
          (adjoin-term
           t1 (add-terms (rest-terms L1) L2)))
         ((< (order t1) (order t2))
          (adjoin-term
           t2 (add-terms L1 (rest-terms L2))))
         (else
          (adjoin-term
           (make-term (order t1)
                      (add (coeff t1) (coeff t2)))
           (add-terms (rest-terms L1)
                      (rest-terms L2)))))))))
  
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
