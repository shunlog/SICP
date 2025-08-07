#lang racket/base

(require teachpacks/racket-turtle)


(define square1
      (list (forward 100)
            (turn-left 90)
            (forward 100)
            (turn-left 90)
            (forward 100)
            (turn-left 90)
            (forward 100)))

(define move
      (list (pen-up)
            (turn-right 90)
            (forward 100)
            (pen-down)
            (change-color "red")))

    (define two-squares
      (list square1
            move
            square1))

(draw-pict two-squares)
