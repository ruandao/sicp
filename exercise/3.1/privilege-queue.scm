
(define (make-privilege-queue)

  (define (empty-queue?)
  (define (dispatch m)
    (cond
     ((eq? m 'empty-queue?) (empty-queue?))
     ((eq? m 'front-queue) (front-queue))
     ((eq? m 'insert-queue!) insert-queue!)
     ((eq? m 'delete-queue!) (delete-queue!))
     (else (error "Unsupport message" m))))
    
  dispatch)

(define (empty-queue? q)
  (q 'empty-queue?))
(define (front-queue q)
  (q 'front-queue))
(define (insert-queue! q privilege item)
  ((q 'insert-queue!) privilege item))
(define (delete-queue! q)
  (q 'delete-queue!))
