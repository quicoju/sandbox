#lang racket

(provide
         id?   ;; Any -> Boolean : Id 
         id=?  ;; Id Id -> Boolean 
         (struct-out player)
         (struct-out body)
         get-score
         PLAYER-FATTEN-DELTA
         WIDTH HEIGHT CUPCAKE PLAYER-SIZE 
         SCORE GOTO SERIALIZE         
         GOTO-LENGTH)

(struct player (id body waypoints) #:prefab)
(struct body (size loc) #:prefab #:mutable)

;; Message ID Constants
(define SCORE 'score)
(define SERIALIZE 'state)
(define GOTO 'goto)
(define GOTO-LENGTH 3)

;; Shared Logical Constants
(define WIDTH 1000)
(define HEIGHT 700)
(define CUPCAKE 15)
(define PLAYER-SIZE (* 3 CUPCAKE)) 
(define PLAYER-FATTEN-DELTA 5)

(define id? string?)
(define id=? string=?)

;; Number -> Number ;; move to serer 
;; gets aplayers score given its fatness
(define (get-score f)
  (/ (- f PLAYER-SIZE) PLAYER-FATTEN-DELTA))
