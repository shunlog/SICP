#lang sicp

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

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


(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2))
            (zip (cdr lst1) (cdr lst2)))))
;; (equal? (zip '(1 2 3) '(a b))
;;         '((1 . a) (2 . b)))


(#%provide enumerate-interval
         accumulate
         fold-right
         fold-left
         filter
         flatmap
         zip)
