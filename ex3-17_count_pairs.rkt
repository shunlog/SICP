#lang sicp

;;; Set data structure:
;;; A set is represented with a list.
;;; An empty set is represented with the pair (nil . nil)

;;; Add an element to the set if it doesn't exist,
;;; Return #t if the elem was added,
;;;   or #f if it already was in the set
(define (set-add s elem)
  (define (iter p)
    (cond 
      ((eq? (car p) elem)
           #f)
          ((null? (cdr p))
           (set-cdr! p (list elem))
           #t)
          (else (iter (cdr p)))))
  (if (null? (car s))
      (begin (set-car! s elem)
             s)
      (iter s)))


;;; I think I get what Perlis meant by
;;; "better 100 methods on 1 data structure":
;;; Because i represented a set with the built-in list,
;;; I can reuse the built-in "length" for my set data structure

(define s (cons nil nil))

(set-add s 1)
(set-add s 2)
(set-add s 1)

(define l1 '(a b))
(define l2 '(a b))
(define l1-copy l1)
(set-add s l1)
(set-add s l2)
(set-add s l1-copy)

(length s)


;;; Count pairs in a list structure (no cycles allowed)
;;; by storing all the pairs in a set
(define (count-pairs x)
  (define s (cons nil nil))
  (define (iter l)
    (cond ((not (pair? l)) nil)
          (else (set-add s l)
                (iter (car l))
                (iter (cdr l)))))
  (iter x)
  (length s))

(define p3 '(a))
(define p2 (cons p3 p3))
(define p1 (cons p2 p3))
(count-pairs p1)                        ; => 3



;;; Return true if the given list contains a cycle
;;; We can map each pair to the list of pairs it can access,
;;; and stop when a pair can access itself
