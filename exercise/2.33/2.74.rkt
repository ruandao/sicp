;#lang planet neil/sicp 
#lang racket
(require (planet soegaard/sicp:2:1/sicp))
(define wave einstein)

;; how division's file should structured
;;a '((name1 . rest-of-record) (name2 . rest-of-record) ...)
(define (get-record name file)
  (define (read-file-as-list file)
    ;; 没有io操作的具体信息, 先这样
    file)
  (define (find-employee-from-list name records)
    (cond ((null? records) '())
          ((eq? name (car (car records))) (car records))
          (else (find-employee-from-list name (cdr records)))))
  (find-employee-from-list name (read-file-as-list file)))

;;b '(name ('salary salary)
(define (get-salary record)
  (define (tag-of-TCs tag-content) (car tag-content))
  (define (content-of-TCs tag-content) (cdr tag-content))
  (define (find-content-for-tag tag tag-contents)
    (cond ((null? tag-contents) '())
          ((eq? tag (tag-of-TCs (car tag-contents))) (content-of-TCs (car tag-contents)))
          (else (find-content-for-tag tag (cdr tag-contents)))))
        
  (if (null? (cdr record))
      '()
      (find-content-for-tag 'salary (cdr record))))

;;c
(define (find-employee-record name list-of-file)
  (cond ((null? list-of-file) '())
        (else (let ((first-record (get-record name (car list-of-file)))
                    (other-records (find-employee-record name (cdr list-of-file))))
                (if (null? first-record)
                    other-records
                    (cons first-record other-records))))))

;;d
;;nothing need to change