
(define (install-spare-poly-package)
  
  (define (negation L)
    (cond
      ((empty-termlist? L) L)
      (else (map (lambda (x) (make-term (order x) (- (coeff x)))) L))))
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (tag x)
    (attach-tag 'poly-spare x))
  (put 'empty-termlist? '(poly-spare)  (lambda(term-list) (empty-termlist? term-list)))
  (put 'negation 'poly-spare (lambda (x) (tag (negation x))))
  (put 'first-term '(poly-spare) (lambda (term-list) (tag (first-term term-list))))
  (put 'rest-terms '(poly-spare) (lambda (term-list) (tag (rest-terms term-list))))
  (put 'order '(poly-spare) order)
  (put 'coeff '(poly-spare) coeff)
  (put 'adjoin-term '(poly-spare poly-spare) (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'make-term 'poly-spare (lambda (order coeff) (tag (make-term order coeff))))
  (put 'the-empty-termlist 'poly-spare (lambda () (tag (the-empty-termlist))))
  'done)
