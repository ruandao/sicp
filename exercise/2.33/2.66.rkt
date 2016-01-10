;#lang planet neil/sicp 
#lang racket
(require (planet soegaard/sicp:2:1/sicp))
(define wave einstein)

(define (lookup  key bt-data)
  (cond ((empty? bt-data) false)
        ((> key (key-of-root bt-data)) (lookup key (right-branch bt-data)))
        ((= key (key-of-root bt-data)) (entry-of-root bt-data))
        ((< key (key-of-root bt-data)) (lookup key (left-branch bt-data)))))