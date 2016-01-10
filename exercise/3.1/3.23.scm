(restart 1)

(define (make-deque)
  (let ((front-ptr '())
	(rear-ptr '()))

    (define (make-pair item prefix after)
      (list item prefix after))
    (define (item-pair pair)
      (car pair))
    (define (prefix-pair pair)
      (cadr pair))
    (define (after-pair pair)
      (caddr pair))
    (define (set-prefix-pair! pair prefix)
      (set-car! (cdr pair) prefix))
    (define (set-after-pair! pair after)
      (set-car! (cddr pair) after))
    (define (empty-pair? item)
      (eq? item '()))
    
    (define (empty-deque?)
      (eq? front-ptr '()))
    (define (front-deque)
      (cond
       ((empty-deque?)
	(error "FRONT called with empty deque"))
       (else (item-pair front-ptr))))
    (define (rear-deque)
      (cond
       ((empty-deque?)
	(error "REAR called with empty deque"))
       (else (item-pair rear-ptr))))
    
    (define (front-insert-deque! item)
      (let ((new-pair (make-pair item '() front-ptr)))
	(cond
	 ((empty-deque?)
	  (set! front-ptr new-pair)
	  (set! rear-ptr new-pair))
	 (else
	  (set-prefix-pair! front-ptr new-pair)
	  (set! front-ptr new-pair)))))
    (define (rear-insert-deque! item)
      (let ((new-pair (make-pair item rear-ptr '())))
	(cond
	 ((empty-deque?)
	  (set! front-ptr new-pair)
	  (set! rear-ptr new-pair))
	 (else
	  (set-after-pair! rear-ptr new-pair)
	  (set! rear-ptr new-pair)))))
    (define (front-delete-deque!)
      (cond
       ((empty-deque?)
	(error "FRONT-DELETE-DEQUE! called with empty queue!"))
       ((empty-pair? (after-pair front-ptr))
	;; 总共只有一个元素
	(set! front-ptr '())
	(set! rear-ptr '()))
       (else
	(set! front-ptr (after-pair front-ptr))
	(set-after-pair! (prefix-pair front-ptr) '())
	(set-prefix-pair! front-ptr '()))))
    (define (rear-delete-deque!)
      (cond
       ((empty-deque?)
	(error "REAR-DELETE-DEQUE! called with empty queue!"))
       ((empty-pair? (prefix-pair rear-ptr))
	;; 总共只有一个元素
	(set! front-ptr '())
	(set! rear-ptr '()))
       (else
	(set! rear-ptr (prefix-pair rear-ptr))
	(set-prefix-pair! (after-pair rear-ptr) '())
	(set-after-pair! rear-ptr '()))))

    (define (print)
      (define (list-elements l)
	(cond
	 ((empty-pair? l) '())
	 (else (cons (item-pair l)
		     (list-elements (after-pair l))))))
      (display (list-elements front-ptr)))

    (define (dispatch message)
      (cond
       ((eq? message 'empty-deque?) (empty-deque?))
       ((eq? message 'front-deque) (front-deque))
       ((eq? message 'rear-deque) (rear-deque))
       ((eq? message 'front-insert-deque!) front-insert-deque!)
       ((eq? message 'rear-insert-deque!) rear-insert-deque!)
       ((eq? message 'front-delete-deque!) (front-delete-deque!))
       ((eq? message 'rear-delete-deque!) (rear-delete-deque!))
       ((eq? message 'print) (print))
       (else (error "Unsupport message" message))))
    dispatch))

(define t (make-deque))
((t 'front-insert-deque!) 'a)
((t 'front-insert-deque!) 'b)
((t 'rear-insert-deque!) 'c)
((t 'rear-insert-deque!) 'd)
(t 'print)
(t 'front-delete-deque!)
(t 'rear-delete-deque!)
(t 'print)
