#lang sicp

(#%require "math.rkt")
(#%require "lists.rkt")


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;; To do nested for-loops,
;;; we combine a flatmap with a map


;;; Generates a sequence of pairs (i, j)
;;; where 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) 
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 5)


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))


;;; (x, y) -> (x, y, (x + y))
(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))
(prime-sum-pairs 5)


;;; Using threading
(#%require threading)

(~> (unique-pairs 5)
    (filter prime-sum? _)
    (map make-pair-sum _))




;;; Ex 2.41

;;; Returns all triples (i, j, k)
;;; Such that 1 <= i < j < k <= n
(define (ordered-triples n)
  (flatmap (lambda (k)
             (flatmap (lambda (j)
                        (map (lambda (i)
                               (list i j k))
                             (enumerate-interval 1 (dec j))))
                      (enumerate-interval 1 (dec k))))
           (enumerate-interval 1 n)))

(ordered-triples 4)

;;; with for-loops

(#%require (only racket/base for*/list)
           compatibility/mlist)

;;; for/list returns immutable list,
;;; but in SICP we use mutable lists
(define (triples2 n)
  (list->mlist
   (for*/list ([k (enumerate-interval 1 n)]
               [j (enumerate-interval 1 (dec k))]
               [i (enumerate-interval 1 (dec j))])
     (list i j k))))
(triples2 4)
