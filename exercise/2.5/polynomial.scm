
(load "apply-generic.scm")

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (make-var symbol level)
  ((get 'make 'var) symbol level))

(define (install-polynomial-package)

  (define (make-var symbol level)
    (cons symbol level))
  (define (level var)
    (cdr var))
  (define (symbol var)
    (car var))
  (define (higher-var var1 var2)
    (higher (level var1) (level var2)))
  
  (define (make-item term coeff)
    (cons term coeff))
  (define (coeff item)
    (cdr item))
  (define (term item)
    (car item))
  (define (higher term1 term2)
    (higher (var term1) (var term2)))

  (define (mul-item item1 item2)
    (make-item (mul (term item1) (term item2))
	       (mul (coeff item1) (coeff item2))))
  ;;
  (define (make-term var power)
    (if (=zero? power)
	(list 'term-leaf '() power)
	(list 'term-leaf var power)))
  (define (term-leaf? term)
    (eq? (car term) 'term-leaf))
  (define (var term-leaf)
    (cadr term-leaf))
  (define (power term-leaf)
    (caddr term-leaf))
  (define (higher-term term1 term2)
    (cond
     ((null? (var term1)) false)
     ((null? (var term2)) true)
     (else (higher (var term1) (var term2)))))
  
  (define (mul-term terms1 terms2)
    (cond
     ((null? terms1) terms2)
     ((null? terms2) terms1)
     ((term-leaf? terms1) (mul-term (cons terms1 '())
				   terms2))
     ((term-leaf? terms2) (mul-term terms1
				   (cons terms2 '())))
     ((higher (car terms1) (car terms2))
      (cons (car terms1) (mul-term (cdr terms1) terms2)))
     ((higher (car terms2) (car terms1))
      (cons (car terms2) (mul-term terms1 (cdr terms1))))
     (else
      (cons (make-term (var (car terms1))
		       (add (power (car terms1))
			    (power (car terms2))))
	    (mul-term (cdr terms1) (cdr terms2))))))
    
  (define (empty-item? item)
    (=zero? (coeff item)))
  (define (add-polys poly1 poly2)
    (cond
     ((null? poly1) poly2)
     ((null? poly2) poly1)
     (else
      (let ((item1 (car poly1))
	    (item2 (car poly2)))
	(cond
	 ((higher (term item1) (term item2))
	  (cons item1 (add-polys (cdr poly1)
				 poly2)))
	 ((higher (term item2) (term item1))
	  (cons item2 (add-polys poly1
				 (cdr poly2))))
	 (else
	  (let ((new-item
		 (make-item (term item1)
			    (add (coeff item1) (coeff item2))))
		(rest-items (add-polys (cdr poly1)
				       (cdr poly2))))
	    (if (not (empty-item? new-item))
		(cons new-item rest-items)
		rest-items))))))))

  (define (negative-poly poly)
    (cond
     ((null? poly) '())
     (else
      (let ((item (car poly)))
	(cons (make-item (term item)
			 (negative (coeff item)))
	      (negative-poly (cdr poly)))))))
  
  (define (sub-polys poly1 poly2)
    (add-polys poly1 (negative-poly poly2)))

  (define (mul-polys poly1 poly2)
    (reduce add-polys
	    '()
	    (flatmap (lambda(item1)
		       (map (lambda(item2)
			      (mul item1 item2))
			    poly1))
		     poly2)))
  ;;
  ;; (define (div-polys poly1 poly2))

  ;; 单变量多项式
  (define (make-poly var terms)
    (reduce add-polys
	    '()
	    (map (lambda(power-coeff)
		   ((get 'make 'polynomial-item)
		    (make-term var (car power-coeff))
		    (cdr power-coeff)))
		 terms)))
  
  ;; interface
  (define (tag-f tag fun)
    (define (f . args)
      (attach-tag tag (f . args)))
    f)

  (put 'make 'var (tag-f 'var make-var))
  (put 'higher '(var var) higher-var)
  
  (put 'higher '(polynomial-item-term polynomial-item-term) higher-term)
  (put 'mul '(polynomial-item-term polynomial-item-term)
       (tag-f 'polynomial-item-term mul-term))
  (put 'make 'polynomial-item-term
       (tag-f 'polynomial-item-term make-term))
  
  (put 'mul '(polynomial-item polynomial-item)
       (tag-f 'polynomial-item mul-item))
  (put 'make 'polynomial-item (tag-f 'polynomial-item make-item))

  (put 'add '(polynomial polynomial) (tag-f 'polynomial add-polys))
  (put 'sub '(polynomial polynomial) (tag-f 'polynomial sub-polys))
  (put 'mul '(polynomial polynomial) (tag-f 'polynomial mul-polys))
  (put 'div '(polynomial polynomial) (tag-f 'polynomial div-polys))
  (put 'make 'polynomial (tag-f 'polynomial make-poly))
  
  'done)
