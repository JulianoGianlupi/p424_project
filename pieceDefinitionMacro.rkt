#lang racket

(struct move (type list))
(struct jump (rank file dist))
;; dummy simetries
(define full-symmetry
  'empty)
(define vertical-symmetry
  'empty)
(define horizontal-symmetry
  'empty)
;(define-syntax-rule (swap x y)
#;
(define-syntax-rule (piece name define-move)
  ())

; a b c symmetry mv

(define-syntax-rule (move-set y x symmetry-type move-type)
  (define action-type
    (cond
      [(string=? move-type "m") (let [action "only move"])]
      [(string=? move-type "c") (let [action "only capture"])]
      [(string=? move-type "mc") (let [action "move or capture"])]
      [(string=? move-type "cm") (let [action "move or capture"])]))
  
  (define apply-move
    (cond
      [(zero? symmetry-type) (full-symmetry (move action (list (jump 1 0 0))))]
      [(positive? symmetry-type) (vertical-symmetry (move action (list (jump 1 0 0))))]
      [(negative? symmetry-type) (horizontal-symmetry (move action (list (jump 1 0 0))))])
    )
  #;(cond
    [(string=? move-type "m") (let [action "only move"])]
    [(string=? move-type "c") (let [action "only capture"])]
    [(string=? move-type "mc") (let [action "move or capture"])]
    [(string=? move-type "cm") (let [action "move or capture"])])
  
  #;(cond
    [(zero? symmetry-type) (full-symmetry (move action (list (jump 1 0 0))))]
    [(positive? symmetry-type) (vertical-symmetry (move action (list (jump 1 0 0))))]
    [(negative? symmetry-type) (horizontal-symmetry (move action (list (jump 1 0 0))))])
  )

; A Piece is a [ListOf Movement] such as:

(define rook   (full-symmetry (move "move or capture" (list (jump 1 0 0))))) 
(define bishop (full-symmetry (move "move or capture" (list (jump 1 1 0)))))
(define knight (full-symmetry (move "move or capture" (list (jump 1 2 1)))))
(define queen  (append rook bishop))
(define king   (append (full-symmetry (move "move or capture" (list (jump 1 0 1))))
                       (full-symmetry (move "move or capture" (list (jump 1 1 1))))))
(define pawn   (append (list (move "only move" (list (jump 1 0 1))))
                       (vertical-symmetry (move "only capture" (list (jump 1 1 1))))))

; The symmetry functions have the type:
; Movement -> [ListOf Movement]

; The above can be written more succinctly using macros:
#|
(piece R [(1,0)+]*)
(piece B [(1,1)+]*)
(piece N [(1,2)]*)
(piece Q R B)
(piece K [(1,0)]* [(1,1)]*)
(piece P m[(1,0)] c[(1,1)]v)|#