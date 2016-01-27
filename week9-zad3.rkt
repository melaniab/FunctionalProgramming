#lang racket


(define (increasing? lst)   
  (cond [(<= (length lst) 1) #t]   
        [(> (first lst) (second lst)) #f]   
        [else (increasing? (rest lst))]))   
 
(define (sublists-at ind lst)   
  (define (helper count res it)   
    (cond [(= (- count 1) (- (length lst) ind)) res]   
          [(= count 0) (helper (+ 1 count) res it)]   
          [else (helper (+ count 1) (append res (list (take it count))) it)]))   
  (helper 0 (list) (drop lst ind)))    
 
;всички подредици от определена позиция натам
(define (all-subseq pos lst)   
  (cond [(= pos (length lst)) '()]   
        [else (append (sublists-at pos lst) (all-subseq (add1 pos) lst))]))   
 
(define (lis ns)   
  (apply max (map length (filter increasing? (all-subseq 0 ns)))))