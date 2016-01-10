;#lang planet neil/sicp 
#lang racket
(require (planet soegaard/sicp:2:1/sicp))
(define wave einstein)


;; type-tag, contents, attach-tag
(define (attach-tag tag contents)
  (if (number? contents)
      contents
      (cons tag contents)))

(define (type-tag value)
  (cond ((number? value) 'scheme-number)
        ((and (pair? value)
              (symbol? (car value)))
         (car value))
        (else (error "Unknown tag -- TYPE-TAG" value))))

(define (contents value)
  (cond ((number? value) value)
        ((and (pair? value)
              (symbol? (car value)))
         (cdr value))
        (else (error "Unknown contents -- CONTENTS" value))))

