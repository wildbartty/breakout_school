#lang racket
(require syntax/parse/define)

(define-syntax push
  (syntax-rules ()
    ((_ lst elem ...)
     (if (list? lst)
         (set! lst (append (list elem ...) lst))
         (set! lst (append (list elem ...) (list lst)))))))

(define-syntax pop
  (syntax-rules ()
    ((_ lst)
     (let ((ret (car lst)))
       (begin
         (set! lst (cdr lst))
         ret)))))

(define test (list 1 2 3 4))

(set! test 0)