#lang racket
(define (take-while p items) 

(cond
  [(empty? items) '()]
  [(not (p (car items))) '()]
	[else (cons (car items) (take-while p (cdr items)))] ))

(define (drop-while p items) 
 (cond [(empty? items) '()]
       [(not(p (car items))) items]
	[else (drop-while p (cdr items))]
))

(define (group lst) ;group mi e greshen
  (cond [(empty? lst) lst]
        [else (list (take-while (λ(x) (equal? x (car lst))) lst)
                    (group (drop-while (λ(x) (equal? x (car lst))) lst)))]))

(define (run-length-encode str)
  (define (encode charlst)
  (define (encode-iter  i charlst)
    (cond [(empty? charlst) '()]
    [(= (length charlst) 1) (cons i charlst)]
    [else (encode-iter (+ i 1)(cdr charlst))]))
    (encode-iter 1 charlst))
  (map encode (group (string->list str))))
         