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
(define (apply-generic1 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length type-tags) 2)
              (let ((tag1 (car type-tags))
                    (tag2 (cdr type-tags)))
                (let ((type1->type2 (get-coercion tag1 tag2))
                      (type2->type1 (get-coercion tag2 tag1)))
                  (cond ((same-type? tag1 tag2)
                         (error "No method for these types -- APPLY-GENERIC"
                                (list op type-tags)))
                        (type1->type2
                         (apply-generic op (type1->type2 (car args)) (cdr args)))
                        (type2->type1
                         (apply-generic op (car args) (type2->type1 (cdr args))))
                        (else (error "No method for these types -- APPLY-GENERIC"
                                     (list op type-tags))))))
              (error "No method for these types -- APPLY-GENERIC"
                     (list op type-tags)))))))

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

(define no-need-coercion "no need coercion")
(define (no-need-coercion? proc) (eq? no-need-coercion proc))
(define (coercion-fail? procs) (eq? procs false))
(define (make-coercion-procs p1 rest-procs)
  (cond 
    ((null? p1) false)
    ((coercion-fail? rest-procs) false)
    (else (cons p1 rest-procs))))
(define (coercion-procedures types type)
  (if (null? types)
      null
      (let ((t1 (car types))
            (t2 type))
        (if (same-type? t1 t2)
            (make-coercion-procs no-need-coercion (coercion-procedures (cdr types) type))
            (make-coercion-procs (get-coercion t1 t2) (coercion-procedures (cdr types) type))))))

(define (find-coercion-procs types)
  (define (find types-need-coercion available-types)
    (if (null? available-types)
        false
        (let ((procs (coercion-procedures types-need-coercion (car available-types))))
          (if procs
              procs
              (find types-need-coercion (cdr available-types))))))
  (cond 
    ((and (> (length types) 1)
          (not (same-types? types)))
     (find types types))
    (else false)))
(define (coercion procs args)
  (define (coercion-1 procs args)
    (cond
      ((null? procs) null)
      ((no-need-coercion? (car procs)) (car args))
      (else (cons ((car procs) (car args)) (coercion-1 (cdr procs) (cdr args))))))
  (cond ((= (length procs) (length args))
         (coercion-1 procs args))
        (else (error "length procs not equal to length args -- COERCION"
                     (list procs args)))))



(define (apply-generic2 op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coercion-procs (find-coercion-procs type-tags)))
            (if coercion-procs
                (apply apply-generic (cons op (coercion coercion-procs args)))
                (error "No method for these types"
                       (list op type-tags))))))))

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
  (define (=zero? x)
    (= (numer x) 0))
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
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
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