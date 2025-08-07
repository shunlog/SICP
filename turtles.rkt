#lang racket
(require graphics/value-turtles
         graphics/value-turtles-examples
         )

(require threading)

(define t (turtles 100 100))


;;; turtles? (-> turtles? turtles?) -> turtles?
;;; Returns the drawing (f t)
;;; but with the turtles and pen from t
(define (return t f)
  (define color (turtles-pen-color t))
  (define width (turtles-pen-width t))
  (define state (turtle-state t))
  (~> (f t)
      (restore-turtle-state state)
      (set-pen-color color)
      (set-pen-width width)))


(~> t
    (return (λ~>
              (set-pen-color "red")
              (draw 200 _)
              (set-pen-width 3)
              (regular-poly 6 100 _)))
    (turn -90 _)
    (draw 200 _)
    (set-pen-color "blue")
    (spyro-gyra _)
    turtles-pict)


(define (multiply-spaced n tv f)
  (cond
    [(zero? n) tv]
    [else
     (merge
      (multiply-spaced 
       (sub1 n)
       (f tv)
       f)
      tv)]))

(~> t
    (draw 50 _)
    (multiply-spaced 10 _ (λ (t) (turn -3 (draw 10 t))))
    (turn -90 _)
    (set-pen-color "green")
    (draw 100 _)
    (return _ (λ~>
               (set-pen-color "blue")
               (set-pen-width 0.2)
               (multiply-spaced 10 _ (λ (t) (turn 20 (draw 10 t))))))
    (turn -90 _)
    (set-pen-color "red")
    (draw 100 _)
    turtles-pict)


;;; turtle? -> turtle?
(define (tree n t)
  (define (iter n depth t)
    (define (draw-branch t)
      (define length
        (+ 10 (* 4 (/ n (add1 depth)))))
      (draw length t))
    (define (maybe-iter t)
      (define chance
        (* 0.5 (/ n (+ 1 depth))))
      (if (< chance (random))
          t
          (iter (sub1 n) (add1 depth) t)))
    (define angle
      (+ 10 (/ 25 (+ 1 depth))))
    (if (= n 0)
        t
        (~> t
            (set-pen-width (+ 1 (* 0.3 n)))
            draw-branch
            (turn angle _)
            (return (λ (t) (maybe-iter t)))
            (turn (* -2 angle) _)
            (return (λ (t) (maybe-iter t))))))
  (return t (λ (t) (iter n 0 t))))

(~> t
    (turn 90 _)
    (tree 12 _)
    (turn -90 _)
    (move -100 _)
    (draw 200 _)
    turtles-pict)
