#lang racket
(define (run-length-encode str)
  (let ([l (string->list str)])
  ;(define (encode-iter i l)
  ;(cond [ (null? l) l]
       ; [ (= (length l) 1) (append l (list i))]
       ; [ else (if (equal? (car l) (cadr l)) (encode-iter (+ i 1) (cdr l))
                   (append (list (car l)) (list (char i)) (encode-iter 1 (cdr l))))]))
 ; (encode-iter 1 l)))