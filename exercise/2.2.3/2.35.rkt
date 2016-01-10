#lang planet neil/sicp

(define (accumulate op initialize seqs)
  (cond ((null? seqs) initialize)
        (else (op (car seqs) (accumulate op initialize (cdr seqs))))))
(define (fringe t)
  (define (_append items result)
    (cond ((null? items) result)
          ((not (pair? items)) (cons items result))
          (else (_append (car items) (_append (cdr items) result)))))
  (_append t nil))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (cond ((null? x) 0)
                               ((not (pair? x)) 1)
                               (else (count-leaves x)))) t)))
(define (count-leaves2 t)
  (accumulate (lambda (x y) (+ 1 y)) 0 (fringe t)))

(define t (list (list 3 5) (list 2 4)))
(fringe t)
(count-leaves t)
(count-leaves2 t)