#lang racket

(require "shared.rkt")

;; Join state
(struct join (clients [time #:mutable]))

;; Play state
(struct play (players food spectators) #:mutable)

;; Internal representation for iPlayers
;; this representation holds a representation
;; of player (defined on shared.rkt)
(define-values
  (ip ip? ip-id ip-iw ip-body ip-waypoints ip-player)
  (let ()
    (struct ip (id iw body waipoints player))
    (define (create iw id body waypoints)
      (ip id iw body waypoints (player id body waypoints)))
    (values
     create ip? ip-id ip-iw ip-body ip-waypoints ip-player)))


