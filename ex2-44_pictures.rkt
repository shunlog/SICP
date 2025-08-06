#lang sicp
(#%require sicp-pict)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                               (- n 1))))
        (below painter 
               (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))



(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) 
                        quarter)))
      (below (flip-vert half) half))))


(paint (square-limit mark-of-zorro 2)
       #:width 400
       #:height 400)


;;; Ex 2.45

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define (square-limit2 painter n)
  (let ((combine4 
         (square-of-four flip-horiz 
                         identity
                         rotate180 
                         flip-vert)))
    (combine4 (corner-split painter n))))

(paint (square-limit2 mark-of-zorro 2)
       #:width 400
       #:height 400)

;;; Defining a general split
;;; that creates two copies combined with combiner-small,
;;; and combines that with the original using combiner-big

(define (split combiner-big combiner-small)
  (define (new-split painter n)
    (if (= n 0)
       painter
       (let ((smaller (new-split painter 
                                (- n 1))))
         (combiner-big painter 
                       (combiner-small smaller smaller)))))
  new-split)


(define right-split2 (split beside below))
(define up-split2 (split below beside))

(paint (beside (right-split2 einstein 2) (up-split2 einstein 2)))

;;; Fibonacci?

(define (fib painter n)
  (if (= n 0)
      painter
      (let ([recursive (fib painter (dec n))])
        (beside painter
                (below painter recursive)))))

(paint ((square-of-four flip-horiz 
                        identity
                        rotate180 
                        flip-vert)
        (fib mark-of-zorro 10)))


;;; painter -> painter
(define (kaleidoscope painter)
  ((square-of-four
    flip-vert 
    rotate180
    identity 
    flip-horiz)
   (fib painter 10)))

(paint (kaleidoscope einstein))


(define (compose1 f1 f2)
  (lambda (arg)
    (f1 (f2 arg))))

;;; transform-painter (painter) vs. make-relative-frame (frame)
(define (transform-painter-relative p orig corner1 corner2)
  (compose1 p (make-relative-frame orig corner1 corner2)))

(paint (transform-painter-relative einstein
                                   (make-vect 0.3 -0.3)
                                   (make-vect 0.5 0.5)
                                   (make-vect 0 0.5)))

;;; painter: (frame? -> any)
;;; paint: (painter? -> any)

;;; painter, real, real -> painter
;;; Returns a new painter that scales the painter's frame
;; with centering
(define (scale1 p mx my)
  (define (scale-frame f)               ; frame -> frame
    (define f-origin (frame-origin f))
    (define f-edge1 (frame-edge1 f))
    (define f-edge2 (frame-edge2 f))
    ;; can't have sequential defines because of #lang sicp
    (let ()
      (define new-e1 (vector-scale mx f-edge1))
      (define new-e2 (vector-scale my f-edge2))
      (let ()
        (define dx (vector-sub f-edge1 new-e1))
        (define dy (vector-sub f-edge2 new-e2))
        (make-frame
         (vector-add f-origin (vector-scale 0.5 (vector-add dx dy)))
         new-e1
         new-e2)
        )))
  (lambda (f) (p (scale-frame f))))

;;; Use the more general transform-painter instead
(define (scale2 p mx my)
  (define inset-x (/ (- 1.0 mx) 2))
  (define inset-y (/ (- 1.0 my) 2))
  (transform-painter p
                     (make-vect inset-x inset-y)
                     (make-vect (- 1.0 inset-x) inset-y)
                     (make-vect inset-x (- 1.0 inset-y))))

(paint (kaleidoscope
        (scale2 einstein 0.8 0.8))
       #:width 400
       #:height 200)


;;; Sierpinski triangle

;;; -> painter
(define triangle
  (vects->painter (list (make-vect 0 0)
                        (make-vect 1.0 0)
                        (make-vect 0.5 1.0)
                        (make-vect 0 0))))

;;; natural -> painter
(define (sierpinski n)
  (define (half-width p)
    (transform-painter p
                       (make-vect 0.25 0.0)
                       (make-vect 0.75 0.0)
                       (make-vect 0.25 1.0)))
  (if (= n 0)
      triangle
      (let ([smaller (sierpinski (dec n))])
        (below (beside smaller smaller)
               (half-width smaller)))))

;;; Reference implementation of superpose
(define (over p1 p2)
  (lambda (frame) (begin (p1 frame)
                         (p2 frame))))

(paint (superpose white (sierpinski 7))
       #:width 500
       #:height 500)


;;; Recursive tunnel


; painter
(define border
  (vects->painter
   (list (make-vect 1.0 1.0)
         (make-vect 1.0 0.0)
         (make-vect 0.0 0.0)
         (make-vect 0.0 1.0)
         (make-vect 1.0 1.0))))

;;; natural -> painter
(define (tunnel n)
  (if (= n 0)
      border
      (superpose (scale2 (tunnel (dec n)) 0.8 0.8)
                 border)))

(paint (superpose gray (tunnel 30)) #:height 250)



(define (spiral n shift)
  (define spire
    (vects->painter
    (list (make-vect 0.5 0.0)
          (make-vect 1.0 0.5)
          (make-vect 0.5 1.0)
          (make-vect 0.0 0.5)
          (make-vect 0.5 (/ shift 2)))))
  (if (= n 0)
      spire
      (superpose (scale2 (spiral (dec n) shift)
                         (- 1.0 shift)
                         (- 1.0 shift))
                 spire)))
(paint (spiral 50 0.2))


;;; Hexagons


;;; three lines spread out from the center,
;; 120 degrees in between
(define ymid 1/3)
(define (corner)
  (define mid (make-vect 0.5 ymid))
  (segments->painter
   (list (make-segment mid (make-vect 0.5 1.0))
         (make-segment mid (make-vect 1 0))
         (make-segment mid (make-vect 0 0)))))

(define (shift-origin p dx dy)
  (define orig (make-vect dx dy))
  (transform-painter p
                     orig
                     (vector-add orig (make-vect 1.0 0))
                     (vector-add orig (make-vect 0 1.0))))

;;; painter that doesn't paint anything
(define (empty f) nil)

;;; float in range [0.0, 1.0] -> one of the options
(define (random-pick opt1 opt2 chance)
  (if (> chance (random 1.0)) opt1 opt2))


(define (hexagons n)
  (define scalefactor 0.612)
  ;; origin is one of [#f 'top 'right 'left]
  (define (_hexagons n flipv? origin)
    (define (dscale p)
      (scale2 p scalefactor scalefactor))
    (define current (if flipv? (corner)
                        (flip-vert (corner))))
    (define dy (+ 1.1)) ; why??? depends on ymid and scalefactor
    (define (left) (shift-origin
                    (_hexagons (dec n) (not flipv?) 'right)
                    -1 (if flipv? -1 1)))
    (define (scaled-left) (dscale
                           (shift-origin
                            (_hexagons (dec n) (not flipv?) 'right)
                            (- 1) (if flipv? (- dy) dy))))
    (define (right) (shift-origin
                     (_hexagons (dec n) (not flipv?) 'left)
                     1 (if flipv? -1 1)))
    (define (scaled-right) (dscale
                            (shift-origin
                             (_hexagons (dec n) (not flipv?) 'left)
                             1 (if flipv? (- dy) dy))))
    (define (bottom) (shift-origin
                      (_hexagons (dec n) (not flipv?) 'top)
                      0 (if flipv? 1 (- 1))))
    (define (scaled-bottom) (dscale
                             (shift-origin
                              (_hexagons (dec n) (not flipv?) 'top)
                              0 (if flipv? 1 (- 1)))))
    (define chance-scaled 0.8)
    (define chance-missing 0.2)
    (define (missing?) (< (random 1.0) chance-missing))
    (if (= n 0)
        current
        (superpose
         current
         (if (or (missing?) (eq? 'left origin)) empty
             ((random-pick scaled-left left chance-scaled)))
         (if (or (missing?) (eq? 'right origin)) empty
             ((random-pick scaled-right right chance-scaled)))
         (if (or (missing?) (eq? 'top origin)) empty
             ((random-pick scaled-bottom bottom chance-scaled))))))
  (_hexagons n #t #f))

(paint (superpose white (scale2 (hexagons 8) 0.2 0.2))
       #:width 600
       #:height 600)
