(restart 1)

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond
	 ((empty-queue?)
	  (set! front-ptr new-pair)
	  (set! rear-ptr new-pair)
	  front-ptr)
	 (else
	  (set-cdr! rear-ptr new-pair)
	  (set! rear-ptr new-pair)
	  front-ptr))))
    (define (delete-queue!)
      (cond
       ((empty-queue?)
	(error "DELETE called with an empty queue" front-ptr))
       (else
	(set! front-ptr (cdr front-ptr))
	front-ptr)))

    (define (empty-queue?)
      (eq? front-ptr '()))
    (define (front-queue)
      (cond
       ((empty-queue?) (error "FRONT called with an empty queue" front-ptr))
       (else (car front-ptr))))
    (define (dispatch x)
      (cond
       ((eq? x 'empty-queue?) (empty-queue?))
       ((eq? x 'front-queue) (front-queue))
       ((eq? x 'insert-queue!) insert-queue!)
       ((eq? x 'delete-queue!) (delete-queue!))
       (else (list "No support message" x))))
    dispatch))

(display "hello world")
(define t (make-queue))
((t 'insert-queue!) 'a)
((t 'insert-queue!) 'b)
(t 'delete-queue!)
(t 'delete-queue!)
(display "end hello world")
;; 写完了, 比对了一下下面的
;; 一个问题是, 我有没有必要, 对 front-ptr 进行包裹一下
;; 譬如加个 set-front-ptr 函数
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond
     ((empty-queue? queue)
      (set-front-ptr! queue new-pair)
      (set-rear-ptr! queue new-pair)
      queue)
     (else
      (set-cdr! (rear-ptr queue) new-pair)
      (set-rear-ptr! queue new-pair)
      queue))))

(define (delete-queue! queue)
  (cond
   ((empty-queue? queue)
    (error "DELETE! called with an empty queue" queue))
   (else
    (set-front-ptr! queue (cdr (front-ptr queue)))
    queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

;; 因为, 显示的queue 其实是一个指向queue 头, 一个指向queue尾部的结构,
;; 然后头部的结构,顺便显示了整个queue

(define (print-queue queue)
  (front-ptr queue))

(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))
