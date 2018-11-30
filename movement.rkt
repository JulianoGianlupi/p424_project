#lang racket



; A Jump is a (jump rank:Int file:Int dist:Nat) where:
; - rank denotes how many ranks the piece jumps forward
; - file denotes how many files the piece jumps rightwards
; - dist denotes how many times the piece jumps (0 is ∞)

(struct jump (rank file dist))

; A Movement is a (move type:Type list:[ListOf Jump]) where:
; - type denotes the type of movement
; - list denotes the list of jumps

; A Type is one of:
; - "only move"
; - "only capture"
; - "move or capture"
; We can add more types later on.

(struct move (type list))

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

;; dummy simetries
(define full-symmetry
  'empty)
(define vertical-symmetry
  'empty)
#|
(piece R [(1,0)+]*)
(piece B [(1,1)+]*)
(piece N [(1,2)]*)
(piece Q R B)
(piece K [(1,0)]* [(1,1)]*)
(piece P m[(1,0)] c[(1,1)]v)|#
