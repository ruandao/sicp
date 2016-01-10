;#lang planet neil/sicp 
#lang racket
(require (planet soegaard/sicp:2:1/sicp))
(define wave einstein)


(define (union-set set1 set2)
  (define (union-list l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((> (car l1) (car l2)) (cons (car l1) (union-list (cdr l1) l2)))
          ((= (car l1) (car l2)) (cons (car l1) (union-list (cdr l1) (cdr l2))))
          ((< (car l1) (car l2)) (cons (car l2) (union-list l1 (cdr l2))))))
  (let ((list-1 (tree-list-2 set1))
        (list-2 (tree-list-2 set2)))
    (list->tree (union-list list-1 list-2))))

(define (intersection-set s1 s2)
  (define (intersection-list l1 l2)
    (cond ((null? l1) '())
          ((null? l2) '())
          ((> (car l1) (car l2)) (intersection-list (cdr l1) l2))
          ((= (car l1) (car l2)) (cons (car l1) (intersection-list (cdr l1) (cdr l2))))
          ((< (car l1) (car l2)) (intersection-list l1 (cdr l2)))))
  (let ((l1 (tree->list-2 s1))
        (l2 (tree->list-2 s2)))
    (list->tree (intersection-list l1 l2))))