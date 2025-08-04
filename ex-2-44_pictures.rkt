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

(paint (right-split2 einstein 2))
(paint (up-split2 einstein 2))

;;; Playing with make-relative-frame

(define (compose1 f1 f2)
  (lambda (arg)
    (f1 (f2 arg))))

(paint (compose1 einstein (make-relative-frame
                                  (make-vect 0 0)
                                  (make-vect 0.5 0.5)
                                  (make-vect 0 0.5)
                                  )))
