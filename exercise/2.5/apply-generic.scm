
(load "basic.scm")

(define (attach-tag tag content)
  (if (eq? tag 'scheme-number)
      content
      (list tag content)))
(define (type-tag x)
  (if (number? x)
      'scheme-number
      (car x)))
(define (contents x)
  (if (number? x)
      x
      (cadr x)))
(define (tag-f tag f)
  (define (fun . args)
    (attach-tag tag (f . args)))
  fun)

;; same-types?
;; raise-to-same
(define (same-types? types)
  (define (same-types-to types type)
    (cond
     ((null? types) true)
     ((eq? (car types) type) (smae-types-to (cdr types) type))
     (else false)))
  (same-types-to types (car types)))

(define (raise arg)
  (apply-generic 'raise arg))
(define (raise-to-type arg type)
  (if (same-type? (type-tag arg) type)
      arg
      (raise-to-type (raise arg) type)))

(define (raise-to-same args)
  (let ((highest-type (reduce (lambda(x y)
				(if (higher x y)
				    x
				    y))
			      (type-tag (car args))
			      (map type-tag args))))
    (reduce (lambda(arg args)
	      (let ((new-arg (raise-to-type arg highest-type)))
		(if (and new-arg
			 (not (eq args false)))
		    (cons new-arg args)
		    false)))
	    '()
	    args)))
		    

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (and (> (length args) 1)
		   (not (same-types? type-tags)))
	      (let ((same-type-args (raise-to-same args)))
		(if same-type-args
		    (apply apply-generic (cons op same-type-args))
		    (error "No method of operation"
			   (list op args))))
	      (error "No method of operation"
		     (list op args)))))))
