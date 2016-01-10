;#lang planet neil/sicp 
#lang racket
(require (planet soegaard/sicp:2:1/sicp))
(define wave einstein)


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? obj)
  (eq? (car obj) 'leaf))

(define (symbol-leaf leaf)
  (cadr leaf))
(define (weight-leaf leaf)
  (caddr leaf))

  
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;(define sample-tree
;  (make-code-tree (make-leaf 'A 4)
;                  (make-code-tree
;                   (make-leaf 'B 2)
;                   (make-code-tree (make-leaf 'D 1)
;                                   (make-leaf 'C 1)))))

;(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;(define sample-symbol (decode sample-message sample-tree))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-1 symbol tree)
    (cond ((leaf? tree) '())
          ((container? symbol (symbols (left-branch tree)))
           (cons '0 (encode-symbol symbol (left-branch tree))))
          (else (cons '1 (encode-symbol symbol (right-branch tree))))))
  (if (container? symbol (symbols tree))
      (encode-symbol-1 symbol tree)
      (error "bad tree -- CONTAINER? symbol" symbol)))

(define (container? symbol list-of-symbol)
  (cond ((null? list-of-symbol) false)
        ((eq? symbol (car list-of-symbol)) true)
        (else (container? symbol (cdr list-of-symbol)))))


;(encode sample-symbol sample-tree)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge order-set)
  (define (successive-merge-1 x order-set)
    (if (null? order-set)
        x
        (successive-merge (adjoin-set (make-code-tree x (car order-set))
                                      (cdr order-set)))))
  (successive-merge-1 (car order-set) (cdr order-set)))

(define rock-song-1950 (list '(a 2) '(na 16) 
                           '(boom 1) '(sha 3)
                           '(get 2) '(yip 9)
                           '(job 2) '(wah 1)))
(define g-tree (generate-huffman-tree rock-song-1950))


(+
(length (encode '(get a job) g-tree))
(length (encode '(sha na na na na na na na na) g-tree))
(length (encode '(get a job) g-tree))
(length (encode '(sha na na na na na na na na) g-tree))
(length (encode '(wah yip yip yip yip yip yip yip yip yip) g-tree))
(length (encode '(sha boom) g-tree))
)