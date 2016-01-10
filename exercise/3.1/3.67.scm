(load "stream.scm")
(load "stream-integers.scm")
(load "../1.26/fermat-test.scm")

(define (prime-pairs int-pairs)
  (stream-filter (lambda (pair)
		   (prime? (+ (car pair) (cadr pair))))
		 int-pairs))


(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda(x) (list (stream-car s) x))
		 (stream-cdr t))
     (stream-map (lambda(x) (list (stream-car t) x))
		 (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s t)
  (if (stream-null? s)
      t
      (cons-stream (stream-car s)
		   (interleave t (stream-cdr s)))))

#|

((lambda()
   (stream-head 'prime-pairs (prime-pairs (pairs integers integers)) 100)
   ))

; 解答					; ;
Can you make any general comments about the order in which the pairs
are placed into the stream?
规律
a 假设前一项为i, 后一项为j, 那么有j>=i,
b 然后从第2项开始如果将结果奇偶项交错开来看, 那么必然会有奇项的前置为i,

恒不变, 然后后置在变化; 如果将偶数项单独提取出来, 又可以递归 a b

How many pairs precede the pair(1, 100)?
the pair(99, 100)? the pair (100, 100)?
((lambda()
   (stream-length (stream-filter prime?
				 (stream-enumerate-interval 2 101)))
   ))

算出来, 101以内的素数有26个, 也就是以1为前项的有26个元素
那么以2为前项的有26/2 = 13
以3为前项的有13/2 = 6
以4为前项的有6/2 = 3
以5为前项的有3/2 = 1
总计有49项

x 看了下别人的答案, 发现,我自己多假设了一个条件: 符合要求的素数, 事实

证, 最重要的是看题目
恩, 如果只是整数对的话, 从(1,2) 到(1,100)[不包含] 共有98个元素
也就是剩下的元素也是98个, 那么总共有
(1,1) + (1,100) + (1, 2...99) + (2,2)... = 198个

后面的不知道怎么算了,应该是暂时不想推...
|#
