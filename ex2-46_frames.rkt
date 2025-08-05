#lang sicp

(#%require sicp-pict)


;;; Layer 0: vector representation

(define (my-make-vect x y)
  (cons x y))

(define (vect-xcor v)
  (car v))

(define (vect-ycor v)
  (cdr v))


;;; Layer 1: vector operations

(define (vect-add a b)
  (my-make-vect (+ (vect-xcor a) (vect-xcor b))
             (+ (vect-ycor a) (vect-ycor b))))

(define (vect-sub a b)
  (my-make-vect (- (vect-xcor a) (vect-xcor b))
             (- (vect-ycor a) (vect-ycor b))))

(define (vect-scale s v)
  (my-make-vect (* s (vect-xcor v))
             (* s (vect-ycor v))))


;;; Layer 0: frames representaiton


;;; vector, vector, vector -> frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin f)
  (car f))
(define (frame-edge1 f)
  (cadr f))
(define (frame-edge2 f)
  (caddr f))

;; (define (make-frame2 origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))




;;; Layer 2: using vectors and frames


(define (frame-coord-map frame)
  (lambda (v)
    (vect-add
     (frame-origin frame)
     (vect-add 
      (vect-scale (vect-xcor v)
                  (frame-edge1 frame))
      (vect-scale (vect-ycor v)
                  (frame-edge2 frame))))))


((frame-coord-map (make-frame
                   (my-make-vect -0.3 0.3)
                   (my-make-vect 1.0 0.2)
                   (my-make-vect 0 1.0)))
 (my-make-vect 0 0))


;;; Actual drawing:
;;;-----------------
;;; the segments->painter is a primitive procedure
;;; provided by sicp-pict,
;;; and it has a contract so it doesn't accept my DIY segments


;;; Reference definition:
;; (define (segments->painter segment-list)
;;   (lambda (frame)
;;     (for-each
;;      (lambda (segment)
;;        (draw-line
;;         ((frame-coord-map frame) 
;;          (start-segment segment))
;;         ((frame-coord-map frame) 
;;          (end-segment segment))))
;;      segment-list)))

;;; Ex 2.49

;;; 1. Draw the outline of the frame
(paint (vects->painter
        (list (make-vect 1.0 1.0)
              (make-vect 1.0 0.0)
              (make-vect 0.0 0.0)
              (make-vect 0.0 1.0)
              (make-vect 1.0 1.0))))


;;; 2. Draw an X
(paint (segments->painter
  (list  (make-segment (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))
         (make-segment (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0)))))
