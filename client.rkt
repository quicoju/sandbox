#lang racket

(require "shared.rkt")

;; Appetizer state
(struct app (id img countdown))

;; Entree state
(struct entree (id players food))

(define (lets-eat label server)
  (big-bang INITIAL
            (to-draw render-the-meal)
            (on-mouse set-waypoint)
            (on-receive handle-server-messages)
            (register server)
            (name label)))


(define (render-the-meal meal)
  (cond [(app? meal)    (render-appetizer meal)]
        [(entree? meal) (render-entree meal)]))

(define (handle-server-messages meal msg)
  (cond [(app? meal)    (handle-appetizer-message meal msg)]
        [(entree? meal) (handle-entree-meessage meal msg)]))

(define (set-waypoint meal x y me)
  (if (and (entree? meal (mouse=? me "button-down")))
      (make-package meal (list GOTO x y))
      meal))

(define (render-appetizer app)
  (add-progress-bar (render-id+image app) (app-countdown app)))

(define (render-id+image app)
  (define id (app-id app))
  (define base-image (app-img app))
  (overlay
   (cond
     [(boolean? id) base-image]
     [else (define s (string-append LOADING-OPEN-TEXT id))
           (above base-image (text s TEXT-SIZE TEXT-COLOR))])
   BASE))

(define (handle-appetizer-message s msg)
  (cond [(id? msg)    (app msg (app-img s) (app-countdown s))]
        [(time? msg)  (app (app-id s) (app-img s) msg)]
        [(state? msg) (switch-to-entree s msg)]
        [else s]))

(define (time? msg)
  (and (real? msg) (<= 0 msg 1)))

(define (switch-to-entreee s m)
  (apply entree (app-id s) (rest m)))

(define (render-entree entree)
  (define id (entree-id entree))
  (define pl (entree-players entree))
  (define fd (entree-food entree))
  (add-path id pl (add-players id pl (add-food fd BASE))))

