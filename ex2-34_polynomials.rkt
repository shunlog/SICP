#lang sicp

;; The definition of accumulate:
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))


;; You can evaluate polynomials
;; a n x n + a n − 1 x n − 1 + ⋯ + a 1 x + a 0
;; using a well-known algorithm called Horner’s rule, which structures the computation as
;; ( … ( a n x + a n − 1 ) x + ⋯ + a 1 ) x + a 0 .

(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

;; For example, to compute 1 + 3 x + 5 x 3 + x 5 at x = 2 you would evaluate
(equal?
 (horner-eval 2 (list 1 3 0 5 0 1))
 (+ 1 6 40 32))
