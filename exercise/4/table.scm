(define (make-node key value subtable)
  (list key value subtable))
(define (eq-key-node? node key)
  (eq? (car node) key))
(define (value-node node)
  (cadr node))
(define (subtable-node node)
  (caddr node))
(define (set-value-node! node val)
  (set-car! (cdr node) val))




(define (make-table)
  (list 'table))
(define (empty-table? table)
  (null? (cdr table)))
(define (first-node-table table)
  (cadr table))
(define (rest-nodes-table table)
  (cdr table))
(define (add-node-table! table node)
  (set-cdr! table
	    (cons node (cdr table))))
(define (length-table table)
  (cond
   ((empty-table? table) 0)
   (else (+ 1 (length-table (rest-nodes-table table))))))




(define (found-node-f-table keys
			    success-run-f
			    error-run-f
			    table)
  (if (and (not (pair? keys))
	   (null? keys))
      (error "keys was not pair! and not null: " keys))
  (cond
   ((empty-table? table) (error-run-f keys table))
   ((eq-key-node? (first-node-table table) (car keys))
    (if (null? (cdr keys))
	(success-run-f (first-node-table table))
	(found-node-f-table (cdr keys)
			    success-run-f
			    error-run-f
			    (subtable-node (first-node-table table)))))
   (else (found-node-f-table keys
			     success-run-f
			     error-run-f
			     (rest-nodes-table table)))))
   
(define (exist-table? keys table)
  (found-node-f-table keys
		      (lambda(node) true)
		      (lambda(rest-keys rest-table) false)
		      table))
		   
(define (get-table keys table)
  (found-node-f-table keys
		      value-node
		      (lambda(rest-keys rest-table) false)
		      table))

(define (put-table! keys val table)
  (cond
   ((empty-table? table)
    (let ((node (make-node (car keys)
			   false
			   (make-table))))
      (begin
	(add-node-table! table node)
	(put-table! keys val table))))
   (else
    (found-node-f-table keys
			(lambda(node)
			  (set-value-node! node val))
			(lambda(rest-keys rest-table)
			  (put-table! rest-keys
				      val
				      rest-table))
			table))))

