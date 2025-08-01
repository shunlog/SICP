#lang sicp


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))
(define fold-right accumulate)


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(fold-right / 1 (list 1 2 3))
(/ 1 (/ 2 (/ 3 1)))

(fold-left  / 1 (list 1 2 3))
(/ (/ (/ 1 1) 2) 3)


(fold-right list nil (list 1 2 3))
(list 1 (list 2 (list 3 nil)))
'(1 (2 (3 ())))


(fold-left  list nil (list 1 2 3))
(list (list (list nil 1) 2) 3)
'(((() 1) 2) 3)

;;; commutativity guarantees that fold-left and  fold-right produce the same values

(fold-left + 0 (list 1 2 3))
(fold-right + 0 (list 1 2 3))


;;; ex 38

(define (reverse1 sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) nil sequence))
(reverse1 '(1 2 3))
;; (op 1 (op 2 (op 3 nil)))
;; (append (append (append '() (list 1)) (list 2)) (list 3))


(define (reverse2 sequence)
  (fold-left 
   (lambda (x y) (cons y x)) nil sequence))
(reverse2 '(1 2 3))
