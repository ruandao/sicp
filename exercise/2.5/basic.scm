
(define (flatmap op args)
  (reduce append
	  '()
	  (map op args)))
