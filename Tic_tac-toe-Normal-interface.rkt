#lang racket


; (draw-board test-board)

(define (make-readable symbol)
  (cond [(= symbol 0) " "]
        [(= symbol 1) "o"]
        [(= symbol 2) "x"]
        ))

 (define (readable-board board)
   (map make-readable board))


; 1. Definiton of a playground: List with 9 elements:
; 1 - x; Racket plays with 1
; 2 - o; Haskell plays with 2 
; 0 - free field
;indexes from 0 to 9 
;Example: Define test playground
(define test '(1 0 0 0 2 0 2 0 1))


;2. Define helper functons:
;2.1. Take n-th element
(define (nth n lst)
 ; (if (or (> n (length lst)) (< n 1))
   ; (error "Index out of bounds.")
    (if (eq? n 1)
      (car lst)
      (nth (- n 1) (cdr lst))))

;2.2. Get rows, cols, diags
(define (get lst play)
  (map (λ(x) (nth x play)) lst))

;2.3. Count elements = y in list 
(define (count y lst)
  (length
   (filter (lambda (x) (equal? x y)) lst)))

;3.1.Define rows, columns, diagonals as lists of indexes - used for !DEBUG ONLY!
;!! NOT by function because of the small number of needed lists
(define row1 '(1 2 3))
(define row2 '(4 5 6))
(define row3 '(7 8 9))
(define col1 '(1 4 7))
(define col2 '(2 5 8))
(define col3 '(3 6 9))
(define diag1 '(1 5 9))
(define diag2 '(3 5 7))
(define all-sublists (list '(1 2 3) '(4 5 6) '(7 8 9) '(1 4 7) '(2 5 8) '(3 6 9) '(1 5 9) '(3 5 7)))

;3.2. Define corners and center as indexes
(define corner1 1)
(define corner2 3)
(define corner3 9)
(define corner4 7)
(define center 5)
 
;4. Playing the game functions
;4.1.Define Setting an 1 (x) on the playground
;NON DESTRICTIVE
(define (set-1 index play)
  (append (take play (- index 1)) '(1) (drop play index)))
;Destructive function
;(define (set1 index play)
; (begin (set! play (set-1 index play)) play))

(define (set-2 index play)
  (append (take play (- index 1)) '(2) (drop play index)))

;4.2. Check if there are N (1, 2, 3) same symbols and one free in a row, column, or diagonal and the others are free 
(define (has-n? n symbol lst play) ; lst = list of indexes; symbol: 1 or 2; 
 (if (and (= (count symbol (get lst play)) n) (= (count 0 (get lst play)) (- 3 n))) #t #f))


;4.3. Put 1 on a free place in a row, column, diagonal
(define (set-free1 lst play)
  (cond [(= 0 (nth (first lst) play)) (set-1 (first lst) play)]
        [(= 0 (nth (second lst) play)) (set-1 (second lst) play)]
        [else (set-1 (third lst) play)]))
    
  
;5. LET'S PLAY THE GAME! -using Newell and Simon's strategy 
(define (playgame play)
  (cond
    ;5.0. Lost game 
    [(>= (length (filter (λ(x) (has-n? 3 2 x play)) all-sublists)) 1) "Player 2 wins. Play another game."]
  ;5.1.Win: If the player has two in a row, they can place a third to get three in a row.
      [(>= (length (filter (λ(x) (has-n? 2 1 x play)) all-sublists)) 1) (set-free1 (first (filter (λ(x) (has-n? 2 1 x play)) all-sublists)) play)
                                                                       ]
 
  
 ;5.2. Block: If the opponent has two in a row, the player must play the third themselves to block the opponent.
 [(>= (length (filter (λ(x) (has-n? 2 2 x play)) all-sublists)) 1) (set-free1 (first (filter (λ(x) (has-n? 2 2 x play)) all-sublists)) play)]
   
;5.3. Fork: Create an opportunity where the player has two threats to win (two non-blocked lines of 2).
 [(and (= (nth corner3 play) 1) (= (nth corner1 play) 1) (= (nth corner4 play) 0)) (set-1 corner4 play)]
 [(and (= (nth corner3 play) 1) (= (nth corner1 play) 1) (= (nth corner2 play) 0)) (set-1 corner2 play)]
  [(and (= (nth corner2 play) 1) (= (nth corner4 play) 1) (= (nth corner1 play) 0)) (set-1 corner1 play)]
   [(and (= (nth corner2 play) 1) (= (nth corner4 play) 1) (= (nth corner3 play) 0)) (set-1 corner3 play)]

;5.4.Blocking an opponent's fork:
;Option 1: The player should create two in a row to force the opponent into defending, as long as it doesn't result in them creating a fork.
    [(>= (length (filter (λ(x) (has-n? 1 1 x play)) all-sublists)) 1) (set-free1 (first (filter (λ(x) (has-n? 1 1 x play)) all-sublists)) play)]
;Option 2: If there is a configuration where the opponent can fork, the player should block that fork.
   [(and (= (nth corner3 play) 2) (= (nth corner1 play) 2) (= (nth corner4 play) 0)) (set-1 corner4 play)]
 [(and (= (nth corner3 play) 2) (= (nth corner1 play) 2) (= (nth corner2 play) 0)) (set-1 corner2 play)]
  [(and (= (nth corner2 play) 2) (= (nth corner4 play) 2) (= (nth corner1 play) 0)) (set-1 corner1 play)]
   [(and (= (nth corner2 play) 2) (= (nth corner4 play) 2) (= (nth corner3 play) 0)) (set-1 corner3 play)]
 
;5.5.Center: A player marks the center. (If it is the first move of the game, playing on a corner gives "O" more opportunities to make a mistake and may therefore be the better choice; however, it makes no difference between perfect players.)
[(= (nth center play) 0) (set-1 center play)]
 ;5.6.Opposite corner: If the opponent is in the corner, the player plays the opposite corner.
[(and (= (nth corner1 play) 2) (= (nth corner3 play) 0)) (set-1 corner3 play)]
[(and (= (nth corner3 play) 2) (= (nth corner1 play) 0)) (set-1 corner1 play)]
[(and (= (nth corner2 play) 2) (= (nth corner4 play) 0)) (set-1 corner4 play)]
[(and (= (nth corner1 play) 2) (= (nth corner2 play) 0)) (set-1 corner2 play)]
;5.7.Empty corner: The player plays in a corner square.
[(= (nth corner1 play) 0) (set-1 corner1 play)]
[(= (nth corner2 play) 0) (set-1 corner2 play)]
[(= (nth corner3 play) 0) (set-1 corner3 play)]
[(= (nth corner4 play) 0) (set-1 corner4 play)]

;5.8. Empty side: The player plays in a middle square on any of the 4 sides.
[(= (nth 2 play) 0) (set-1 2 play)]
[(= (nth 4 play) 0) (set-1 4 play)]
[(= (nth 6 play) 0) (set-1 6 play)]
[(= (nth 8 play) 0) (set-1 8 play)]
[else "Draw, babe. Play another game."]))

(define playground '(0 0 0 0 0 0 0 0 0))
(define playground1 '(0 1 0 0 0 0 0 0 0))
;(readable-board playground1)

(define indexes '(1 2 3 4 5 6 7 8 9))
;slice 
(define (slice l offset n)
  (take (drop l offset) n))


;6. Input-output
(define (input-output play)
  (display "Your index: ")
  (define in (read))
  (cond [(not (number? in))  (begin (display "Wrong output!") (input-output play))]
    [(not (and (> in 0) (<= in 9)))  (begin (display "Wrong output!") (input-output play))]
          [(zero? in) (display "Come later!")]
       [else 
        (begin
         (define playgr (set-2 in play))
       (display "Current game is: ")
      (newline)
       
        (display (readable-board (slice playgr 0 3)))
        (newline)
        (display (readable-board (slice playgr 3 3)))
        (newline)
        (display (readable-board (slice playgr 6 3)))
        (newline)
        (display "My next choice is: ")
        (newline)
         
        (if (list? (playgame playgr)) ;if list is returned
        (begin (display (readable-board (slice (playgame playgr) 0 3)))
        (newline)
        (display (readable-board (slice (playgame playgr) 3 3)))
        (newline)
        (display (readable-board (slice (playgame playgr) 6 3))))
        ;if result is returned
        (display (playgame playgr)))
        (newline)
        ;if player 1 wins after his turn
         (cond [(and (list? (playgame playgr)) (>= (length (filter (λ(x) (has-n? 3 1 x (playgame playgr))) all-sublists)) 1)) (begin (display " Player 1 wins! Let's play another game! \n") (input-output playground))]
        ;if its a draw after player1's turn 
         [(and (list? (playgame playgr)) (equal? '() (filter zero? (playgame playgr)))) (begin (display " It's a draw. Let's play another game!\n")(input-output playground))]
         [else (input-output (playgame playgr))]))]))
        ;(newline)
        ;(input-output (playgame playgr))))

(display "Let's play the game. Imagine that your playground is like that: \n")


(display (slice indexes 0 3))
        (newline)
        (display (slice indexes 3 3))
        (newline)
        (display (slice indexes 6 3))
        (newline)
 (display "Give index from 1 to 9 where you want to put a X. Give 0 for exit.  \n")
(input-output playground)