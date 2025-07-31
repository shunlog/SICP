#lang sicp

;;; If integers, doesn't include high
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;;; P - the predicate function P(x, y)
;;;   which describes a region of space
;;;   such that P(x,y) is true if (x, y) falls in that region
;;; x1, x2, y1, y2 - the bounds of a rectangle which contains the region
;;; trials - number of trials to perform in the Monte Carlo method
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (try)
    (define x (random-in-range x1 x2))
    (define y (random-in-range y1 y2))
    (P x y))
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((try) (iter (dec trials-remaining)
                       (inc trials-passed)))
          (else (iter (dec trials-remaining)
                      trials-passed))))
  (iter trials 0))

;;; -------
;;; Testing

(define (sqr x) (* x x))

;;; Ellipse contained in the rectangle (2, 4) - (8, 10)
(define (ellipseP x y)
  (<= (+ (sqr (- x 5))) (sqr (- y 7))
      (sqr 3)))

(exact->inexact
 (estimate-integral ellipseP
                    2 8 4 10
                    10000))
