
(define (make-monitored f)
  (define counter 0)
  (define (how-many-calls?) counter)
  (define (reset-count)
    (set! counter 0))
  (define (mf x)
    (set! counter (+ counter 1))
    (f x))
  (define (dispatch x)
    (cond
     ((eq? x 'how-many-calls?) (how-many-calls?))
     ((eq? x 'reset-count) (reset-count))
     (else (mf x))))
  dispatch)


(define s (make-monitored sqrt))

(s 100)
(s 'how-many-calls?)
