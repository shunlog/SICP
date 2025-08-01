#lang sicp

(#%require "lists.rkt") 

(define empty-board '())


(define (andmap pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (andmap pred (cdr lst)))
        (else #f)))


;;; A Permutation is a list of queens positions,
;;; such that the index = column and value = row of each queen


(define (unsafe? row1 col1 row2 col2)
    (define dcol (abs (- col1 col2)))                      ; columns between the two
    (or (= row1 row2)
        (= row2 (+ row1 dcol))
        (= row2 (- row1 dcol))))

;;; Returns whether the queen at column k (1-indexed)
;;; is not in check with the others in a given Permutation
(define (safe? col1 permutation)  
  (define row1 (list-ref permutation (dec col1)))
  (andmap (lambda (pair)
            (define col2 (car pair))
            (define row2 (cdr pair))
            (not (unsafe? row1 col1 row2 col2)))
          (filter
           (lambda (p) (not (= (car p) col1)))
           (zip (enumerate-interval 1 (length permutation))
                permutation))))

(safe? 2 '(1 3))
(not (safe? 2 '(1 2)))
(safe? 1 '(1 10 2))
(not (safe? 1 '(1 10 3)))
(not (safe? 4 '(1 10 5 10)))



(define (adjoin-permutation new-row k permutation)
  (append permutation (list new-row)))


;;; Returns list of solutions (permutations of size board-size)
(define (queens board-size)
  ;; -> list of permutations
  ;; of placing k queens in the first k columns
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (permutation) 
           (safe? k permutation))
         (flatmap
          (lambda (perm)
            (map (lambda (new-row)
                   (adjoin-permutation 
                    new-row 
                    k 
                    perm))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(= (length (queens 3)) 0)
(= (length (queens 4)) 2)
(= (length (queens 5)) 10)
(= (length (queens 6)) 4)
(= (length (queens 7)) 40)
(= (length (queens 8)) 92)
