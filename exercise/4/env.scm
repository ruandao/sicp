(load "table.scm")

  
(define (make-env)
  ((g-env 'make-env)))

(define (extend-env env)
  ((g-env 'extend-env) env))
(define (parent-env env)
  ((g-env 'parent-env) env))

;; 添加新的变量
(define (add-new-var-env! var val env)
  ((g-env 'add-new-var-env!) var val env))

;; 批量添加新的变量
(define (add-new-vars-env! vars vals env)
  (cond
   ((null? vars) env)
   (else
    (add-new-var-env! (car vars) (car vals) env)
    (add-new-vars-env! (cdr vars) (cdr vals) env))))

;; 更新已有的变量
(define (update-var-env! var val env)
  ((g-env 'update-var-env!) var val env))

;; 更新已有的变量,或者添加新的变量
(define (set-var-val-env! var val env)
  (if (not (update-var-env! var val env))
      (add-new-var-env! var val env)))

;; 查找某个变量
(define (lookup-var-env var env)
  ((g-env 'lookup-var-env) var env))



(define (_env)
  
  (define last-idx 0)
  (define env-table (make-table))
  (define (make-envidx idx)
    (list 'env idx))
  (define (all-envidx env)
    (cdr env))
  (define (current-envidx env)
    (cadr env))
  (define (parent-env env)
    (cond
     ((null? (cddr env)) (list 'env))
     (else (cons 'env (cddr env)))))

  (define (empty-env? env)
    (null? (cdr env)))

  
  (define (extend-env env)
    (define new-env (make-env))
    (set-cdr! new-env (cons (current-envidx new-env)
			    (all-envidx env)))
    new-env)
  
  (define (first-table-env env)
    (cond
     ((empty-env? env) (error "not table for empty env" env))
     (else
      (get-table (list (current-envidx env))
		 env-table))))


  (define (lookup-varnode-env var successf errf env)
    (cond
     ((empty-env? env) (errf))
     (else
      (found-node-f-table (list var)
			  (lambda(node)
			    (successf node))
			  (lambda(rest-vars rest-table)
			    (lookup-varnode-env
			     var
			     successf
			     errf
			     (parent-env env)))
			  (first-table-env env)))))

  ;; 添加新的变量
  (define (add-new-var-env! var val env)
    (put-table! (list var) val (first-table-env env))
    env)
  ;; 更新已有的变量
  (define (update-var-env! var val env)
    (lookup-varnode-env var
			(lambda(node)
			  (begin
			    (set-value-node! node val)
			    true))
			(lambda() false)
			env))
  ;; 查找某个变量
  (define (lookup-var-env var env)
    (lookup-varnode-env var
			(lambda(node) (value-node node))
			(lambda() false)
			env))
  
  
  (define (make-env)
    (set! last-idx (+ last-idx 1))
    (put-table! (list last-idx) (make-table) env-table)
    (make-envidx last-idx))

  (define (dispatch m)
    (cond
     ((eq? m 'make-env) make-env)
     ((eq? m 'extend-env) extend-env)
     ((eq? m 'parent-env) parent-env)
     ((eq? m 'add-new-var-env!) add-new-var-env!)
     ((eq? m 'update-var-env!) update-var-env!)
     ((eq? m 'lookup-var-env) lookup-var-env)
     (else (error "Unsupport env op -- DISPATCH" m))))
  dispatch)


(define g-env (_env))
