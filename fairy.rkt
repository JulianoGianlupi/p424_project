#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(struct move (type rank file dist) #:transparent)

(define (only-move rank file dist)
  (move "only move" rank file dist))

(define (only-capture rank file dist)
  (move "only capture" rank file dist))

(define (move-or-capture rank file dist)
  (move "move or capture" rank file dist))

(define (vertical-symmetry m)
  (match m
    [(move type rank file dist)
     (remove-duplicates (list m (move type rank (- file) dist)))]))

(define (horizontal-symmetry m)
  (match m
    [(move type rank file dist)
     (remove-duplicates (list m (move type (- rank) file dist)))]))

(define (full-symmetry m)
  (match m
    [(move type rank file dist)
     (append-map vertical-symmetry
                 (append-map horizontal-symmetry 
                             (remove-duplicates (list m (move type file rank dist)))))]))

(define rook   (full-symmetry (move-or-capture 1 0 0)))
(define bishop (full-symmetry (move-or-capture 1 1 0)))
(define knight (full-symmetry (move-or-capture 1 2 1)))
(define queen  (append rook bishop))
(define king   (append (full-symmetry (move-or-capture 1 0 1))
                       (full-symmetry (move-or-capture 1 1 1))))
(define pawn   (append (list              (only-move    1 0 2))
                       (vertical-symmetry (only-capture 1 1 1))))

(struct piece (color glyph moves) #:transparent)

(define (white glyph moves)
  (piece "white" (string-upcase glyph) moves))

(define (black glyph moves)
  (piece "black" (string-downcase glyph) (map (lambda (m)
                                           (match m
                                             [(move type rank file dist)
                                              (move type (- rank) file dist)])) moves)))

(define r (black "♜" rook))
(define b (black "♝" bishop))
(define n (black "♞" knight))
(define q (black "♛" queen))
(define k (black "♚" king))
(define p (black "♟" pawn))

(define _ #f)

(define R (white "♖" rook))
(define B (white "♗" bishop))
(define N (white "♘" knight))
(define Q (white "♕" queen))
(define K (white "♔" king))
(define P (white "♙" pawn))

(define chess (list (list r n b q k b n r)
                    (list p p p p p p p p)
                    (list _ _ _ _ _ _ _ _)
                    (list _ _ _ _ _ _ _ _)
                    (list _ _ _ _ _ _ _ _)
                    (list _ _ _ _ _ _ _ _)
                    (list P P P P P P P P)
                    (list R N B Q K B N R)))

(define light (make-color 255 206 158))
(define dark  (make-color 209 139 71))

(define (board size rows from flip)
  (let ([lightsquare (square size "solid" light)]
        [darksquare  (square size "solid" dark)]
        [destination (square size "solid" (color 20 85 30 76))]
        [moves       (get-moves rows from)])
    (apply above (flip-board flip (for/list ([cols (in-list rows)]
                                             [x    (in-naturals)])
                                    (apply beside (flip-board flip (for/list ([square (in-list cols)]
                                                             [y      (in-naturals)])
                                                    (let ([result (overlay (text (if square (piece-glyph square) " ")
                                                                                 (* size 0.8) (if (equal? (cons y x) from) "red" "black"))
                                                                           (if (even? (+ x y))
                                                                               lightsquare
                                                                               darksquare))])
                                                      (if (member (cons y x) moves)
                                                          (overlay destination result)
                                                          result))))))))))
(define (flip-board flip rows)
  (if flip (reverse rows) rows))

(define (get-piece rows from)
  (list-ref (list-ref rows (cdr from)) (car from)))

(define (board-set rows x y value)
  (let ([row (list-ref rows y)])
    (list-set rows y (list-set row x value))))

(define (play-move rows from to)
  (let ([piece (get-piece rows from)])
    (board-set (board-set rows (car from) (cdr from) _)
               (car to) (cdr to) piece)))

(define (get-moves rows from)
  (if (empty? from) empty
      (let* ([piece (get-piece rows from)]
             [moves (piece-moves piece)]
             [color (piece-color piece)])
        (append-map (lambda (m)
                      (match m
                        [(move type rank file dist)
                         (let loop ([x (+ (car from) file)]
                                    [y (- (cdr from) rank)]
                                    [d (if (zero? dist) +inf.0 dist)])
                           (cond [(zero? d) empty]
                                 [(< x 0) empty]
                                 [(< y 0) empty]
                                 [(> x 7) empty]
                                 [(> y 7) empty]
                                 [(get-piece rows (cons x y))
                                  (if (and (or (string=? type "only capture")
                                               (string=? type "move or capture"))
                                           (not (string=? color (piece-color (get-piece rows (cons x y))))))
                                      (list (cons x y))
                                      empty)]
                                 [(or (string=? type "only move")
                                      (string=? type "move or capture"))
                                  (cons (cons x y) (loop (+ x file) (- y rank) (sub1 d)))]
                                 [else (loop (+ x file) (- y rank) (sub1 d))]))])) moves))))

(big-bang (list chess empty #f #f)
  [to-draw  (lambda (state) (board 50 (first state) (second state) (fourth state)))]
  [on-key   (lambda (state key)
              (cond [(key=? key "f") (list (first state) (second state) (third state) (not (fourth state)))]
                    [else state]))]
  [on-mouse (lambda (state x* y* mouse)
              (define x (if (fourth state) (- 400 x*) x*))
              (define y (if (fourth state) (- 400 y*) y*))
              (cond [(not (string=? mouse "button-down")) state]
                    [(empty? (second state))
                     (let* ([file (quotient x 50)]
                            [rank (quotient y 50)]
                            [piece (get-piece (first state) (cons file rank))])
                       (list (first state)
                             (if (and piece (string=? (if (third state) "black" "white")
                                                      (piece-color piece)))
                                 (cons file rank) empty)
                             (third state)
                             (fourth state)))]
                    [else (let* ([file (quotient x 50)]
                                 [rank (quotient y 50)]
                                 [dest (cons file rank)]
                                 [moves (get-moves (first state) (second state))]
                                 [piece (get-piece (first state) dest)])
                            (cond [(member dest moves) (list (play-move (first state) (second state) dest)
                                                             empty (not (third state)) (fourth state))]
                                  [(equal? dest (second state)) (list (first state) empty (third state) (fourth state))]
                                  [else (list (first state)
                                              (if (and piece (string=? (if (third state) "black" "white")
                                                                       (piece-color piece)))
                                                  (cons file rank) empty)
                                              (third state) (fourth state))]))]))])
