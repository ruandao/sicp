(restart 1)

(load "~/sicp/3.1/connector.scm")
(load "~/sicp/3.1/connector-probe.scm")
(load "~/sicp/3.1/connector-adder.scm")
(load "~/sicp/3.1/connector-multiplier.scm")


(define (averager a b c)
  (define sum (make-connector))
  (define div2 (make-connector))
  (adder a b sum)
  (multiplier sum div2 c)
  (constant 1/2 div2)
  'ok)


(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(averager a b c)
(probe 'a a)
(probe 'b b)
(probe 'c c)
(set-value! a 3 'user)
(set-value! b 8 'user)
