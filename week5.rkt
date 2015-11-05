#lang racket
(define (build-list2 n f)

(define (build-iter i n f)

 (if (= n i) '()	(cons (f i) (build-iter (+ i 1) n f))))
(build-iter 0 n f))

(define (append2 l1 l2) 
( cond [(and (empty? l1) (not (empty? l2))) (cons (car l2) (append2 l1 (cdr l2)))]
[(empty? l2) l1]
[else (cons (car l1) (append2 (cdr l1) l2))]))

; Взима първите n елемента от даден списък ; Ако (> n (length items)), тогава връща items ; -> (take2 3 (list 1 2 3 4 5)) ; '(1 2 3)

 (define (take2 n items) 
( cond [(>= n (length items)) items]
	[ (= n 0) '()]
	[else (cons (car items) (take2 (- n 1) (cdr items)))]))

; Маха първите n елемента от даден списък ; Ако (> n (length items)) връща '() ; -> (drop2 3 (list 1 2 3 4 5)) ; '(4 5) 

(define (drop2 n items) 
( cond [(>= n (length items)) '()]
	[(= n 0)  items]
[(= n 1) (cdr items)]
	[else (drop2 (- n 1) (cdr items))]))

; Функция от по-висок ред. Взима поредни елементи от items докато предиката p за тях дава истина ; -> (take-while zero? (list 0 0 0 1 2 3)) ; '(0 0 0)

(define (take-while p items) 

(cond [(not (p (car items))) '()]
	[else (cons (car items) (take-while p (cdr items)))] ))

(define (drop-while p items) 
 (cond [(not(p (car items))) items]
	[else (drop-while p (cdr items))]
))

(define (number->list n)
(define (reversedNlist n)
(if (= (quotient n 10) 0) (cons n '())
	(cons (remainder n 10) (reversedNlist (quotient n 10)))))
(reverse (reversedNlist n)))

(define (list->number ns) 
(if (= (length ns) 1) (car ns)
   (+ ( * (expt 10 (-(length ns) 1)) (car ns)) (list->number (cdr ns)))))

(define (reverse2 lst)
    (define (reverse-helper lst res)
  (if (empty? lst) res                             
                                 
      (reverse-helper (cdr lst)               
                   (cons (car lst) res))))
    (reverse-helper lst '()))  


