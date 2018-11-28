#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define light (make-color 255 206 158))
(define dark  (make-color 209 139 71))

(define (board size rows)
  (let ([lightsquare (square size "solid" light)]
        [darksquare  (square size "solid" dark)])
    (apply above (for/list ([cols (in-list rows)]
                            [x    (in-naturals)])
                   (apply beside (for/list ([square (in-list cols)]
                                            [y      (in-naturals)])
                                   (overlay (text square (sub1 size) "black")
                                            (if (even? (+ x y))
                                                lightsquare
                                                darksquare))))))))

(define chess '(("r" "n" "b" "q" "k" "b" "n" "r")
                ("p" "p" "p" "p" "p" "p" "p" "p")
                (" " " " " " " " " " " " " " " ")
                (" " " " " " " " " " " " " " " ")
                (" " " " " " " " " " " " " " " ")
                (" " " " " " " " " " " " " " " ")
                ("P" "P" "P" "P" "P" "P" "P" "P")
                ("R" "N" "B" "Q" "K" "B" "N" "R")))


(define (flip-board rows)
  (reverse (map reverse rows)))

(define (board-set rows x y value)
  (let ([row (list-ref rows y)])
    (list-set rows y (list-set row x value))))

(define (play-move rows from to)
  (let ([piece (list-ref (list-ref rows (cdr from)) (car from))])
    (board-set (board-set rows (car from) (cdr from) " ")
               (car to) (cdr to) piece)))

(big-bang (cons chess empty)
  [to-draw  (lambda (state) (board 30 (car state)))]
  [on-key   (lambda (state key)
              (cond [(key=? key "f") (cons (flip-board (car state)) (cdr state))]
                    [else state]))]
  [on-mouse (lambda (state x y mouse)
              (cond [(not (string=? mouse "button-down")) state]
                    [(empty? (cdr state)) (cons (car state) (cons (quotient x 30) (quotient y 30)))]
                    [else (cons (play-move (car state) (cdr state) (cons (quotient x 30) (quotient y 30))) empty)]))])
