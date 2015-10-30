#lang racket
(define (f p g h)
  (lambda (x)
    (and (p (g x)) (p (h x)))))

 