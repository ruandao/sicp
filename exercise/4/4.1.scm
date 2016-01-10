
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (list-of-values-rl exps env)
  (define (rl left-exp vals env)
    (cons (eval left-exp env)
	  vals))
  (if (no-operands? exps)
      '()
      (rl (first-operand exps)
	  (list-of-values-lr (rest-operands exps) env)
	  env)))
  

(define (list-of-values-lr exps env)
  (define (lr left-val right-exps env)
    (cons left-val
	  (list-of-values-lr right-exps env)))
  (if (no-operands? exps)
      '()
      (lr (eval (first-operand exps) env)
	  (rest-operands exps)
	  env)))
