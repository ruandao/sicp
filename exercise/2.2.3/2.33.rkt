#lang planet neil/sicp

(define (accumulate op initialize seq)
  (cond ((null? seq) initialize)
        (else (op (car seq) (accumulate op initialize (cdr seq))))))
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))


(define (square x) (* x x))
(define l (list 1 2 4 5))

(define (println x)
  (display x)
  (newline))
(println (map square l))

(println l)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(println (append l l))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(println (length l))
