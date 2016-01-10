(load "utils.scm")
(load "table.scm")
(load "env.scm")
(load "true.scm")


(define debug false)
(define (log x)
  (if debug
      (display x)))
(define (logln)
  (if debug
      (newline)))



;; 查询类型 的函数匹配列表
(define queryTagList '())
					; 插入 (put-type-query! queryFun tag)
(define (put-type-query! query tag)
  (set! queryTagList (cons (cons query tag)
			   queryTagList)))
(define (query-type exp qList)
  (cond
   ((null? qList) false)
   (((caar qList) exp) (cdar qList))
   (else (query-type exp (cdr qList)))))

(define (type-tag exp)
  (query-type exp queryTagList))



					; 查询第一个symbol 是否是对应的tag
					; (tagged-list? exp tag)
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


;; 标签, 函数 表
(define procTable (make-table))
					; 插入 (put-proc! tags proc)
(define (put-proc! tags proc)
  (put-table! tags proc procTable))
(define (get-proc tags)
  (let ((get-result (get-table tags procTable)))
    (if get-result
	get-result
	(error "can't get-proc: " tags))))


(define (eval exp env)
  (log "eval ")(log exp)(logln)
  (let ((tag (type-tag exp)))
    (if tag
	(let ((proc (get-proc (cons 'eval tag))))
	  (if proc
	      (let ((val (proc exp env)))
		(logln)
		(log "eval: ")(log exp)(logln)
		(log "tag: ")(log tag)(logln)
		(log "val: ")(log val)(logln)
		val)
	      (error
	       "Unknown type tag -- EVAL" (list tag exp))))
	(if (pair? exp)
	    (let ((proc (eval (car exp) env)))
	      (if proc
		  (eval (cons proc (cdr exp)) env)
		  (error "first element was #f -- EVAL" exp)))
	    (error "Can't found type tag -- EVAL" exp)))))



(define env (make-env))

