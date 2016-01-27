#lang racket
(define (take int lst)
  (cond [(= int 0) '()]
        [(empty? lst) '()]
        [else (cons (car lst) (take (- int 1) (cdr lst)))]))

(define (drop int lst)
  (cond [(= int 0) lst]
        [(empty? lst) '()]
        [else (drop (- int 1) (cdr lst))]))

(define (consify n lst) ; 2 zad 
  (cond [(empty? lst) lst]
        [else (cons (take n lst) (consify n (drop n lst)))]))


