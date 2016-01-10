
(define (put x) x)
(define (get x) x)

(define (attach-tag tag contents)
  (if (number? contents)
      contents
      (cons tag contents)))

(define (type-tag x)
  (cond ((number? x) 'scheme-number)
	((and (pair? x)
	      (symbol? (car x)))
	 (car x))
	(else (error "Unknown tag -- TYPE-TAG" x))))

(define (contents x)
  (cond ((number? x) x)
	((and (pair? x)
	      (symbol? (car x)))
	 (cdr x))
	(else (error "Unknown tag -- TYPE-TAG" x))))




(define (make-rational n d)  ((get 'make 'rational) n d))
(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y)(apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (high x y) (apply-generic 'high x y))
(define (project x) (apply-generic 'project x))
(define (project? x) (get 'project (type-tag x)))

(define (drop x)
  (if (project? x)
      (if (equ? (raise (project x)) x)
	  (drop x)
	  x)
      x))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (negation x)
  (apply-generic 'negation x))

(define (height? x y)
  (> (get 'level x) (get 'level y)))
(define (heightest-type types)
  (define (heightest-type-1 types type)
    (cond
     ((null? types) type)
     ((height? type (car types)) (heightest-type-1 (cdr types) type))
     (else (heightest-type-1 (car types)))))
  (height-type-1 types (car types)))

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
	  (proc (map contents args))
	  (if (and (> (length args) 1)
		   (not (same-types? type-tags)))
	      (let ((same-type-args (raise-to-same-type (map drop
							     args))))
		(if same-type-args
		    (apply apply-generic (cons op same-type-args))
		    (error "No method for these types"
			   (list op type-tags))))
	      (error "No method for these types"
		     (list op type-tags)))))))
