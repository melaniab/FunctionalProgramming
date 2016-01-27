#lang racket
;take while
(define (take-while p items)
    (if (or (empty? items) (not (p (first items)))) (list)
      (cons (first items) (take-while p (rest items)))))

;drop while
(define (drop-while p items)
    (if (or (empty? items) (not (p (first items)))) items
      (drop-while p (rest items))))

;group
(define (group lst)
  (cond [(empty? lst) lst]
        [else (cons (take-while (λ(x) (equal? x (car lst))) lst)
                    (group (drop-while (λ(x) (equal? x (car lst))) lst)))]))

(define (les lst)
  (apply max (map length (group lst))))
  