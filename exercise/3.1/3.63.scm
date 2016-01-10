
(load "stream.scm")

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
		 (stream-map (lambda(guess)
			       (sqrt-improve guess x))
			     guesses)))
  guesses)

(define (sqrt-stream-1 x)
  (cons-stream 1.0
	       (stream-map (lambda(guess)
			     (sqrt-improve guess x))
			   (sqrt-stream-1 x))))

#|

 (stream-head '(sqrt 2) (sqrt-stream 2) 10)
 (stream-head '(sqrt 2) (sqrt-stream-1 2) 10)
; a
; 两个版本的`sqrt` 结果都是一致的, 但是,
; `sqrt-stream` 是直接使用了上一个计算的结果
; `sqrt-stream-1` 则是计算下一项是, 重新计算了前面的所有项
;      然后每个项还重新计算前面的所有项
; b
; 使用优化的memo-proc和没优化的没有差别, 因为他是直接重新计算了



|#
