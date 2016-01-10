
(define (install-polynomial-package)

  ;; add-poly
  (define (add-poly p1 p2)
    (reduce adjoin-item p1 p2))
  ;; mul-poly
  (define (mul-poly p1 p2)
    (reduce adjoin-item
	    '()
	    (map (lambda (item1)
		   (reduce (lambda(item2, items)
			     (adjoin-item (mul item1 item2) items))
			   '()
			   p1))
		 p2)))

  ;; add-item
  (define (add-item item1 item2)
    (cond
     ((higher (term item1) (term item2)) (cons item1 item2))
     ((higher (term item2) (term item2)) (cons item2 item1))
     (else (make-item (term item1)
		      (add (coeff item1) (coeff item2))))))
  ;; mul-item
  (define (mul-item item1 item2)
    (make-item (mul (term item1) (term item2))
	       (mul (coeff item1) (coeff item2))))
  ;; make-item
  (define (make-item term coeff)
    (if (=zero? coeff)
	(list 'item (unvisible-term) coeff)
	(list 'item term coeff)))
  (define (term item) (cadr item))
  (define (coeff item) (caddr item))
  (define (item? item) (eq? (car item) 'item))
  ;; make-term
  (define (make-term var power)
    (list 'term var power))
  (define (var term) (cadr term))
  (define (power term) (caddr term))
  (define (term? term) (eq? (car term) 'term)) ;; 判断是否是叶子节点
  (define (higher term1 term2)
    ;; 复合的项, 层级比较高
    (cond
     ((or (term? term1) (term? term2))
      (and (term? term2)
	   (or (not (term? term1))
	       (higher (level (var term1)) (level (var term2))))))
     ((higher (car term1) (car term2)) true)
     ((higher (car term2) (car term1)) false)
     (else ;; 二者相等
      (higher (cdr term1) (cdr term2)))))
      

  (define (unvisible-term)
    (make-term (unvisible-var) 0))
  (define (unvisible-term? x)
    (eq? (power x) 0))
  (define (adjoin-term term terms)
    (cond
     ((null? terms) (cons term '()))
     (else
      (let ((first-term (car terms))
	    (rest-terms (cdr terms)))
	(cond
	 ((higher (level (var term))
		  (level (var first-term))) (cons term terms))
	 ((higher (level (var first-term))
		  (level (var term)))
	  (cons first-term (adjoin-term term rest-terms)))
	 (else (cons (make-term (var term)
				(add (power term) (power first-term)))
		     rest-terms)))))))
      
  (define (mul-term terms1 terms2)
    (cond
     ((unvisible-term? terms1) terms2)
     ((unvisible-term? terms2) terms1)
     (else (reduce adjoin-term terms1 terms2))))

  ;; make-var
  (define (make-var symbol level)
    (list 'var symbol level))
  (define (unvisible-var) (make-var '() -1))
  (define (same-var? x y) (eq? (symbol x) (symbol y)))
  (define (symbol var) (cadr var))
  (define (level var) (caddr var))
  (define (var? var) (eq? (car var) 'var))

  ;; item->poly
  (define (item->poly x)
    (tag-poly (cons x '())))
  ;; interface
  (define (tag-poly x)
    (cond
     ((item? x) (attach-tag 'item x))
     (else (attach-tag 'poly x))))
  (put 'add '(poly poly) (lambda (a b) (tag-poly (add-poly a b))))
  (put 'mul '(poly poly) (lambda (a b) (tag-poly (mul-poly a b))))
  (put 'add '(item item) (lambda (a b) (tag-poly (add-item a b))))
  (put 'mul '(item item) (lambda (a b) (tag-poly (mul-item a b))))
  (put-coercion 'item 'poly item->poly)
  'done)
(define (install-polynimal-package)
  (define (make-var symbol level)
    (cons symbol level))
  (define (level var)
    (cdr var))
  ;; item 
  (define (higher vars1 vars2)
    ;; 复合的优先级比较高
    (cond
     ((empty-var? vars1) false)
     ((empty-var? vars2) true)
     (else (cond
	    ((> (level (car vars1)) (level (car vars2))) true)
	    ((< (level (car vars1)) (level (car vars2))) false)
	    (else (higher (cdr vars1) (cdr vars2)))))))
	     
  (define (make-var symbol level)
    (attach-tag 'single-var (cons symbol level)))
  (define (symbol var)
    (cond
     ((eq? (type-tag var) 'single-var) (car (contents var)))
     (else (error "It's not variable" var))))
  (define (level var)
    (if (eq? (type-tag var) 'single-var)
	(cdr (contents var))
	(error "It's not variable" var)))
    
  ;;
  (define (make-item var coeff)
    (cons var coeff))
  ;;
  (define (variable x)
    (car x))
  ;;
  (define (coeff x)
    (cdr x))
  (define (empty-items? x) (null? x))
  (define (empty-items) '())
  ;;
  (define (higher-var item1 item2)
    (higher (variable item1) (variable item2)))
  ;;
  (define (add item1 item2)
    (make-item (variable item1)
	       (add (coeff item1) (coeff item2))))

  (define (adjoin-items item items)
    (cond
     ((empty-items? items) (cons item '()))
     ((higher-var item (car items)) (cons item items))
     ((higher-var (car items) item) (cons (car items)
				    (adjoin-items item (cdr items))))
     (else (cons (add item (car items))
		 (cdr items)))))
  (define (add-items items1 items2)
    (reduce adjoin-items items1 items2))

  (define (mul-items-items items1 items2)
    (reduce add-items
	    (empty-items)
	    (map (lambda(item)
		   (mul-item-items item items1))
		 items2)))

  (define (mul-item-items item items)
    (reduce add-items
	    (empty-items)
	    (map (lambda(item2) (mul item item2)) items)))

  (define (mul-item-item item1 item2)
    (make-item (mul (variable item1) (variable item2))
	       (mul (coeff item1) (coeff item2))))
  ;;
  (define (mul-var var1 var2))
  ;;
  (define (empty-item))
  (define (variable item) (car item))
  (define (coeff item) (cdr item))
  (define (make-item var coeff)
    (cond
     ((=zero? coeff) (empty-item))
     (else (cons var coeff))))
  ;; interface
  (define (tag-item x) (attach-tag 'var-item x))
  (put 'mul '(var-items var-items) mul-items-items)
  (put 'mul '(var-item var-items) mul-item-items)
  (put 'mul '(var-item var-item) mul-item-item)
  (put 'make-item (lambda (var coeff) (tag-item (make-item var coeff))))
  'done)

(define (install-poly-package)
  (define (add-terms L1 L2)
    (cond
     ((empty-termlist? L1) L2)
     ((empty-termlist? L2) L1)
     (else
      (let ((t1 (first-term L1))
	    (t2 (first-term L2)))
	(cond
	 ((> (order t1) (order t2))
	  (adjoin-terms t1
			(add-terms (rest-terms L1)
				   L2)))
	 ((= (order t1) (order t2))
	  (adjoin-terms (make-term (type-tag L1)
				   (order t1)
				   (add (coeff t1)
					(coeff t2)))
			(add-terms (rest-terms L1)
				   (rest-terms L2))))
	 ((< (order t1) (order t2))
	  (adjoin-terms t2
			(add-terms L1
				   (rest-terms L2)))))))))
				   
  (define (sub-terms L1 L2)
    (add-terms L1 (negation L2)))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist (type-tag L1))
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (type-tag t1)
                      (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
	(list (the-empty-termlist) (the-empty-termlist))
	(let ((t1 (first-term L1))
	      (t2 (first-term L2)))
	  (if (> (order t2) (order t1))
	      (list (the-empty-termlist) L1)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (- (order t1) (order t2))))
		(let ((new-term (make-term (type-tag L1) new-o
					   new-c)))
		  (let ((rest-of-result
			 (div-terms (sub-terms L1
					       (mul-terms new-term
							  L2))
				    L2)))
		    (list (adjoin-term new-term (car rest-of-result))
			  (cdr rest-of-result)))))))))
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable x)
    (car x))
  (define (term x)
    (cdr x))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(map (lambda (x) (make-poly (variable p1) x))
	     (div-terms (term p1) (term p2)))
	(error "quotient and remainder were not same variable"
	       (list p1 p2))))
  (define (add-polys p1 p2)
    (cond
     ((empty-poly? p1) p2)
     ((empty-poly? p2) p1)
     (else
      (let ((item1 (first-item p1))
	    (item2 (first-item p2)))
	(cond
	 ((high-var item1 item2)
	  (adjoin-polys item1 (add-polys (rest-items p1) p2)))
	 ((high-var item2 item1)
	  (adjoin-polys item2 (add-polys p1 (rest-items p2))))
	 (else (adjoin-polys (add item1 item2)
			     (add-polys (rest-vars p1) (rest-vars p2)))))))))
  (define (empty-poly? poly) (null? poly))
  (define (make-var var coeff) (cons var coeff))
							
      

		  
					     
	      
  ;; interface
  (define (empty-termlist? x)
    (apply-generic 'empty-termlist? x))
  (define (first-term term-list)
    (apply-generic 'first-term term-list))
  (define (rest-terms term-list)
    (apply-generic 'rest-terms term-list))
  (define (order term)
    (apply-generic 'order term))
  (define (coeff term)
    (apply-generic 'coeff term))
  (define (adjoin-term term term-list)
    (apply-generic 'adjoin-term term term-list))
  (define (make-term type order coeff)
    ((get 'make-term type) order coeff))
  (define (the-empty-termlist type)
    ((get 'the-empty-termlist type)))
  'done)
