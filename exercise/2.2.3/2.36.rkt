#lang planet neil/sicp

(define (accumulate op initialize sequences)
  (cond ((null? sequences) initialize)
        (else (op (car sequences) (accumulate op initialize (cdr sequences))))))

(define (map proc sequences)
  (accumulate (lambda (x y) (cons (proc x) y)) nil sequences))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define d (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)
                (list 10 11 12)))
(display (accumulate-n + 0 d))