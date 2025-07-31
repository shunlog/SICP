#lang sicp

;;; Data definition layer


;; First constructors:

;; (define (make-mobile left right)
;;   (list left right))

;; (define (make-branch length structure)
;;   (list length structure))


;; Alternative constructors:
;; By changing to cons-es instead of lists,
;; we had to modify only this data definition layer,
;; without touching the domain layer

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; Selectors:

(define mobile? pair?)                ; not quite correct, but will do

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length b)
  (car b))

;;; returns either a mobile or a number (weight)
(define (branch-structure b)
  (cdr b))

;;; Domain layer

(define (branch-weight b)
    (define struct (branch-structure b))
    (if (mobile? struct)
        (total-weight struct)
        struct))

(define (total-weight mob)
  (+ (branch-weight (right-branch mob))
     (branch-weight (left-branch mob))))


;;; A mobile is balanced if:
;;; Each of its sub-mobiles is balanced
;;; L-left x W-left = L-right x W-right
(define (balanced? mob)
  (define (branch-balanced? b)
    (define struct (branch-structure b))
    (if (mobile? struct)
        (balanced? struct)
        #t))
  (define rb (right-branch mob))
  (define lb(left-branch mob))
  (and (branch-balanced? rb)
       (branch-balanced? lb)
       (= (* (branch-weight lb) (branch-length lb))
          (* (branch-weight rb) (branch-length rb)))))


;;; testing

(define m1
  (make-mobile
   (make-branch 1 (make-mobile
                   (make-branch 3 10)
                   (make-branch 4 20)))
   (make-branch 2 30)))

(eq? (total-weight m1) 60)
(eq? (balanced? m1) #f)

(define m2
  (make-mobile
   (make-branch 2 10)
   (make-mobile 1
                (make-mobile
                 (make-branch 1 15)
                 (make-branch 3 5)))))

(eq? (balanced? m2) #t)
