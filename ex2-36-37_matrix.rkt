#lang sicp


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

;;; A Matrix is a List of Lists (rows) of equal lengths

; Matrix -> List
;;; Applies accumulate to each column of the matrix
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define m0 '((1 2 3)
             (4 5 6)
             (7 8 9)
             (10 11 12)))
(equal?
 (accumulate-n + 0 m0)
 '(22 26 30))


;;; Ex. 2.37 

;;; Refresh your algebra with the diagram from wikipedia:
;;; https://en.wikipedia.org/wiki/Matrix_multiplication#Illustration
;;; During Matrix x vector, the vector is represented vertically as a nx1 matrix
;;; During vector x matrix, the vector is horizontal, as a 1xn matrix
;;; It's all just dot-products

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(equal?
 (dot-product '(1 3 -5) '(4 -2 -1)) 3)

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(equal? (matrix-*-vector m0 '(2 0 -1))
        '(-1 2 5 8))


(define (transpose mat)
  (accumulate-n cons '() mat))

(equal? (transpose m0)
        '((1 4 7 10)
          (2 5 8 11)
          (3 6 9 12)))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row-m)
           (map (lambda (col-n)
                  (dot-product row-m col-n))
                cols))
         m)))

(equal? (matrix-*-matrix m0 '((2 1) (0 2) (-1 0)))
        '((-1 5)
          (2 14)
          (5 23)
          (8 32)))
