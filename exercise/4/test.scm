(restart 1)


					; true.scm
((lambda()
   (load "true.scm")
   
   (display (eq? (true? '0)
		 #f))
	    
   (display (eq? (true? '1)
		 #t))
   (display (eq? (true? '#t)
		 #t))
   (display (eq? (true? '#f)
		 #f))
   ))
					; utils.scm
((lambda()
   (load "utils.scm")
   
   (display (eq? (atom? 'a)
		 #t))
   (display (eq? (equal? 'a 'b)
		 #f))
   (display (eq? (equal? '(a b) '(a b))
		 #t))
   (display (eq? (equal? '(a c) '(a b))
		 #f))

   ))
					; table.scm
((lambda()
   (load "table.scm")

   
   (define n (make-node 'key 3 '()))
   (eq-key-node? n 'key)
   (eq? (value-node n) 3)
   (eq? (subtable-node n) '())
   (set-value-node! n 7)
   (eq-key-node? n 'key)


   (define table (make-table))
   (define n1 (make-node 'a 3 '()))
   (define n2 (make-node 'b 4 '()))
   (add-node-table! table n1)
   (eq? (value-node (first-node-table table)) 3)
   (add-node-table! table n2)
   (eq? (value-node (first-node-table table)) 4)
   (eq? (length-table table) 2)



   (define t (make-table))
   (display (eq? (get-table '(a) t) #f))

   (put-table! '(b) 3 t)
   (display (eq? (get-table '(b) t) 3))

   (put-table! '(a b c) 5 t)
   (display (eq? (get-table '(a b c) t) 5))


   ))

					; env.scm

((lambda()
   (load "env.scm")
   
   (define env (make-env))
   
   (display (eq? (lookup-var-env 'a env)
		 #f))

   (set-var-val-env! 'a 3 env)
   (display (eq? (lookup-var-env 'a env)
		 3))

   (set-var-val-env! 'b 4 env)

   (set! env (extend-env env))
   (lookup-var-env 'a env)
   (display (eq? (lookup-var-env 'a env)
		 3))
   (display (eq? (lookup-var-env 'b env)
		 4))
   (add-new-var-env! 'b 9 env)
   (display (eq? (lookup-var-env 'b env)
		 9))
   (display (eq? (lookup-var-env 'b (parent-env env))
		 4))
   
   ))



					; apply-eval.scm
((lambda()
   (load "apply-eval.scm")
   (load "_self-eval.scm")
   (install-self-eval)


   (put-proc! (list 'a 'b) 3)
   (display (eq? (get-proc (list 'a 'b)) 3))



   (define exp '3)
   (define env (list (make-table)))
   (display (eq? (eval exp env) 3))



   (display (tagged-list? (list 'if 'true) 'if))




   (put-type-query! (lambda(exp)
		      (= exp 3))
		    '(is3))
   (display (eq? (car (type-tag 3)) 'is3))

   (display (equal? (type-tag 2)
		    '(self-eval)))


   (put-type-query! (lambda(exp)
		      (= exp 4))
		    '(is4))
   (display (eq? (eq? (car (type-tag 4)) 'is4)
		 #t))
   ))


					; _self-eval.scm

((lambda()
   (load "_self-eval.scm")
   (install-self-eval)
   
   (display (eq? (eval '3 env) 3))
					; 字符串相等怎么匹配???
   ;(eq? (eval '"hello" (make-table)) '"hello") 
   (display (eq? (eval '#t env) #t))
   (display (eq? (eval '#f env) #f))
   ))


					; _variable.scm
((lambda()
   (load "_variable-define.scm")
   (load "_self-eval.scm")
   (install-self-eval)
   (install-variable-define)
   ;; 变量
   ((lambda()
      (define env (make-env))
      (define exp '(define uuu 111))
      (eval exp env)
      (display (eq? (eval 'uuu env)
		    111))
      ))
   
   ;; 赋值
   ((lambda()

      (define exp '(set! uuu 8))
      (define env (make-env))
      (define exp-d '(define uuu 111))
      (eval exp-d env)
      (display (eq? (eval 'uuu env)
		    111))
      
      (set! env (extend-env env))
      (eval exp env)
      (display (eq? (eval 'uuu env)
		    8))
      (define exp-d2 '(define uuu 222))
      (eval exp-d2 env)
      (display (eq? (eval 'uuu env)
		    222))
      ))
   ))


					; _if.scm
((lambda()
   (load "_if.scm")
   (load "_self-eval.scm")
   (install-self-eval)
   (install-if)
   
   
   (define env (make-env))
   (define exp '(if #t 3 8))
   (display (eq? (eval exp env)
		 3))
   
   (define exp1 '(if #f 3 8))
   (display (eq? (eval exp1 env)
		 8))
   ))

((lambda()
   (load "_arithmetic.scm")
   (load "_self-eval.scm")
   (install-self-eval)
   (install-arithmetic)
   (define env (make-env))
   (define exp0 '(+ 1 3))
   (define exp1 '(- 3 1))
   (define exp2 '(* 3 2))
   (define exp3 '(/ 6 3))
   (display (eq? (eval exp0 env)
		 4))
   (display (eq? (eval exp1 env)
		 2))
   (display (eq? (eval exp2 env)
		 6))
   (display (eq? (eval exp3 env)
		 2))
   ))



					; _and-or.scm
((lambda()
   (load "_and-or.scm")
   (load "_self-eval.scm")
   (install-and-or)
   (install-self-eval)
   
   (define exp1 '(and #t #t #f))
   (display (eq? (eval exp1 env)
		 #f))

   (define exp2 '(and #t #f #t))
   (display (eq? (eval exp2 env)
		 #f))
   
   (define exp3 '(and #t #t))
   (display (eq? (eval exp3 env)
		 #t))

   (define exp4 '(or #f #f #t))
   (display (eq? (eval exp4 env)
		 #t))

   (define exp5 '(or #f #f #f))
   (display (eq? (eval exp5 env)
		 #f))
   
   ))
))


					; _begin.scm
((lambda()
   (load "_begin.scm")
   (load "_self-eval.scm")
   (install-begin)
   (install-self-eval)

   (display (eq? (eval '(begin 3 4 5) env)
		 5))
   (display (eq? (eval '(begin 4 5 3) env)
		 3))
   ))

					; _eq.scm
((lambda()
   (load "_eq.scm")
   (load "_self-eval.scm")
   (install-self-eval)
   (install-eq)
   (define env (make-env))
   (define exp '(eq? 1 2))
   (display (eq? (eval exp env)
		 #f))
   (define exp2 '(eq? 1 1))
   (display (eq? (eval exp2 env)
		 #t))
   ))

					; _quoted.scm
((lambda()
   (load "_pair.scm")
   (load "_quote.scm")
   (load "_variable-define.scm")
   (load "_eq.scm")
   (load "_self-eval.scm")
   (install-self-eval)
   (install-pair)
   (install-eq)
   (install-variable-define)
   (install-quote)
   
   (define exp '(quote (a b c)))
   (display (equal? (eval exp env)
		    '(cons (quote a)
			   (cons (quote b)
				 (cons (quote c)
				       (text ()))))))
   
   (define exp1 '(quote ()))
   (display (equal? (eval exp1 env)
		    '(text ())))
   
   (define exp2 '(eq? 'b 'b))
   (display (eq? (eval exp2 env)
		 #t))

   (define exp3 '(define a '()))
   (eval exp3 env)
   (display (equal? (eval 'a env)
		    '(text ())))
   (define exp4 '(null? a))
   (eval exp4 env)
   ))

					; cons-addition.scm
((lambda()
   (load "_pair.scm")
   (load "cons-addition.scm")
   (load "_self-eval.scm")
   (load "_quote.scm")
   (install-quote)
   (install-self-eval)
   (install-pair)
   (install-cons-addition)
   (define env (make-env))

   (define exp '(caar (cons (cons 3 4)
			    '())))
   (display (eq? (eval exp env)
		 3))
   ))


					; _cond.scm
((lambda()
   (load "_cond.scm")
   (load "_self-eval.scm")
   (load "_if.scm")
   (load "_begin.scm")
   (load "_variable-define.scm")
   (load "_lambda.scm")
   (load "cons-addition.scm")
   (load "_pair.scm")
   (load "_eq.scm")
   (load "_quote.scm")
   (load "_apply.scm")
   (install-apply)
   (install-quote)
   (install-eq)
   (install-pair)
   ;(install-cons-addition)
   (install-variable-define)
   (install-begin)
   (install-if)
   (install-self-eval)
   (install-cond)
   (install-lambda)

   (define env (make-env))
   (define exp '(cond (#f 8)
		      (#f 7)
		      (#t 2)))
   (display (eq? (eval exp env)
		 2))

   (define exp1 '(cond (else 8)))
   (display (eq? (eval exp1 env)
		 8))

   (define exp20 '(define a 9))
   (define exp2 '(cond (#f 9)
		       (else (set! a 2) a)))
   (eval exp20 env)
   (display (eq? (eval exp2 env)
		 2))


   (define exp300 '(define (caar p)
		     (car (car p))))
   (define exp301 '(define (cadr p)
		     (car (cdr p))))
   (eval exp300 env)
   (eval exp301 env)
   (define exp30 '(define (assoc key records)
		    (cond
		     ((null? records) false)
		     ((eq? (caar records) key) (car records))
		     (else (assoc key (cdr records))))))
   (define exp3 '(cond
		  ((assoc 'b '((a 1) (b 2))) => cadr)
		  (else false)))
   (eval exp30 env)
   (display (eq? (eval exp3 env)
		 2))

   ))




					; _lambda.scm
((lambda()
   (load "_lambda.scm")
   (load "_self-eval.scm")
   (load "_variable-define.scm")
   (load "_begin.scm")
   (load "_arithmetic.scm")
   (install-arithmetic)
   (install-begin)
   (install-variable-define)
   (install-self-eval)
   (install-lambda)

   (define env (make-env))
   (display (eq? (eval '(+ 1 2) env)
		 3))
   (newline)


   (define a '(define a 3))
   (define exp1 '((lambda(a)
		    (+ 3 1)
		    a) 5))
   (eval a env)
   (eval exp1 env)

		   
   
   ;; 闭包, 作用域循环引用

   (define exp2 '(define y
		   (lambda(a)
		     (lambda(b)
		       (+ a b)))))
   (define exp3 '((y 2) 8))
   (eval exp2 env)
   (eval exp3 env)

   ))


					; _let.scm
((lambda()
   (load "_let.scm")
   (load "_lambda.scm")
   (load "_arithmetic.scm")
   (load "_self-eval.scm")
   (load "_variable-define.scm")
   (load "_begin.scm")
   (install-let)
   (install-lambda)
   (install-self-eval)
   (install-variable-define)
   (install-begin)
   (install-arithmetic)

   
   (define env (make-env))
   (define exp '(let ((a 3) (b 4))
		  (+ a b)))
   (display (eq? (eval exp env)
		 7))
   ))



					; _pair.scm
((lambda()
   (load "_pair.scm")
   (load "_self-eval.scm")
   (load "_variable-define.scm")
   (load "_quote.scm")
   (install-quote)
   (install-variable-define)
   (install-self-eval)
   (install-pair)

   (define env (make-env))
   (define exp '(define a (cons 3 8)))
   (eval exp env)
   (define exp-0 'a)
   (eval exp-0 env)

   (define exp1 '(car a))
   (display (eq? (eval exp1 env)
		 3))

   (define exp2 '(cdr a))
   (display (eq? (eval exp2 env)
		 8))

   (define exp3 '(null? (quote ())))
   (display (eq? (eval exp3 env)
		 #t))
   
   (define exp4 '(pair? (cons 3 4)))
   (display (eq? (eval exp4 env)
		 #t))

   (define exp5 '(pair? (quote ())))
   (display (eq? (eval exp5 env)
		 #f))

   (define exp6 '(pair? (quote b)))
   (display (eq? (eval exp6 env)
		 #f))
   ))
     


					; _let.scm
((lambda()

   (load "_let.scm")
   (load "_lambda.scm")
   (load "_arithmetic.scm")
   (load "_self-eval.scm")
   (load "_variable-define.scm")
   (load "_begin.scm")
   (install-let*)
   (install-lambda)
   (install-self-eval)
   (install-variable-define)
   (install-begin)
   (install-arithmetic)
;   (set! debug true)
   
   (define env (make-env))
   (define exp '(let* ((a 3) (b (+ a 4)))
		  (+ a b)))
   (display (eq? (eval exp env)
		 10))

   (define exp1 '(let* ((x 3)
			(y (+ x 2))
			(z (+ x y 5)))
		   (* x z)))
   (display (eq? (eval exp1 env)
		 39))
   
   ))


					; _cmp.scm
((lambda()
   (load "_cmp.scm")
   (load "_variable-define.scm")
   (load "_self-eval.scm")
   (install-self-eval)
   (install-variable-define)
   (install-cmp)
   
   (define env (make-env))
   (define a '(define a 3))
   (define b '(define b 2))
   (define c '(define c 3))
   (define exp '(= a b))
   (define exp1 '(> a b))
   (define exp2 '(< a b))
   (define exp3 '(= a c))
   (eval a env)
   (eval b env)
   (eval c env)
   (display (eq? (eval exp env)
		 #f))
   (display (eq? (eval exp1 env)
		 #t))
   (display (eq? (eval exp2 env)
		 #f))
   (display (eq? (eval exp3 env)
		 #t))
   ))


					; _let.scm
					; Named let
((lambda()

   (load "_let.scm")
   (load "_lambda.scm")
   (load "_arithmetic.scm")
   (load "_self-eval.scm")
   (load "_variable-define.scm")
   (load "_begin.scm")
   (load "_apply.scm")
   (load "_if.scm")
   (load "_cmp.scm")
   (install-cmp)
   (install-if)
   (install-apply)
   (install-let)
   (install-lambda)
   (install-self-eval)
   (install-variable-define)
   (install-begin)
   (install-arithmetic)
;   (set! debug true)
   
   (define env (make-env))
   (define exp '(define (fib n)
		  (let fib-iter ((a 1)
				 (b 0)
				 (count n))
		    (if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1))))))
   (define exp1 '(fib 6))
   (eval exp env)
   (display (eq? (eval exp1 env)
		 8))
   
   ))

;;; 下面的没写, 先放着,不管他,等看完 csapp 再回头处理
					; _do.scm
((lambda()
   (load "_do.scm")
   (install-do)
   
   (define env (make-env))
   (define exp0 '(define a 0))
   (define exp1 '(define b 3))
   (define exp  '(do ((i 1 (+ i 0))
		      (b 2 (* b 2)))
		     ((> i 4))
		   (display b)))
   
   ))
