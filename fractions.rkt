#lang racket
(define (fst pair)
  (car pair))

(define (snd pair) ;modified
  (car (cdr pair)))

(define (nod a b) ;euclidean algorithm
    ( cond [(simplify-frac '(0 5))(= a b) a]
           [(> a b) (nod (- a b) b)]
           [else (nod a (- b a))]))

(define (nok a b) ; (nok a b) = (/ ( * a b) (nod a b))
  (/ ( * a b) (nod a b)))

(define (add-frac frac1 frac2)
  (let ([k (nok (snd frac2) (snd frac1))])
  (cons (+ (* (fst frac1) (/ k (snd frac1))) (* (fst frac2) (/ k (snd frac2)))) k)))

(define (substract-frac frac1 frac2)
  (let ([k (nok (snd frac2) (snd frac1))])
  (cons (- (* (fst frac1) (/ k (snd frac1))) (* (fst frac2) (/ k (snd frac2)))) k)))

(define (mult-frac frac1 frac2)
  (cons (* (fst frac1) (fst frac2)) (* (snd frac1) (snd frac2))
   ))

(define (simplify-frac frac)
  (let ([d (nod (fst frac) (snd frac))])
    (cons (/ (fst frac) d) (/ (snd frac) d))))