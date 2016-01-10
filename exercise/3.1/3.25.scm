(restart 1)

(define (make-table)
  (define (make-node key value tables)
    (list key value tables))
  (define (value-node node)
    (cadr node))
  (define (key-node node)
    (car node))
  (define (tables-node node)
    (caddr node))
  (define (find-subnode key node)
    (define (find-1 key tables)
      (cond
       ((null? tables) false)
       ((eq? key (key-node (car tables)))
	(car tables))
       (else (find-1 key (cdr tables)))))
    (find-1 key (tables-node node)))
  (define (set-tables-node! node tables)
    (set-car! (cddr node) tables))
  (define (add-subnode! node subnode)
    (set-tables-node! node
		      (cons subnode (tables-node node))))
  (define (set-value-node! node value)
    (set-car! (cdr node) value))

  (define root-node (make-node '() '() '()))

  (define (lookup . args)
    (define (lookup-1 keys node)
      (cond
       ((null? keys) (value-node node))
       (else
	(let ((new-node (find-subnode (car keys) node)))
	  (if new-node
	      (lookup-1 (cdr keys) new-node)
	      false)))))
    (lookup-1 args root-node))

  (define (insert! . args)
    (define (keys-value args)
      (cond
       ((null? (cdr args)) (cons '() (car args)))
       (else
	(let ((nextKeys-value (keys-value (cdr args))))
	  (cons (cons (car args) (car nextKeys-value))
		(cdr nextKeys-value))))))
    (define (insert-1! keys value node)
      (cond
       ((null? keys) (set-value-node! node value))
       (else
	(let ((sub-node (find-subnode (car keys) node)))
	  (if sub-node
	      (insert-1! (cdr keys) value sub-node)
	      (let ((sub-node (make-node (car keys) '() '())))
		(begin
		  (add-subnode! node sub-node)
		  (insert-1! (cdr keys) value sub-node))))))))
    (let ((ks-v (keys-value args)))
      (insert-1! (car ks-v) (cdr ks-v) root-node)))



  (define (dispatch m)
    (cond
     ((eq? m 'lookup) lookup)
     ((eq? m 'insert!) insert!)
     (else (list "Unsupport message" m))))
  dispatch))

(define x-table (make-table))
(define put (x-table 'insert!))
(define get (x-table 'lookup))

(put 'a 'ab 'c 33)
(put 'a 'b 90)
(get 'x 'nn)
(get 'a 'ab 'c)
(get 'a 'b)
