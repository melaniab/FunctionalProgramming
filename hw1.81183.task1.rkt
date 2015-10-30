#lang racket
(define(bc n k)
(cond [( = k 1 ) n]
      [( > k 1 ) ( * ( bc n  (- k 1))
                  ( / ( + ( - n k ) 1)
                     k))]))
                   
