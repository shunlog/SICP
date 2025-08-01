#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))



(define (count-leaves0 x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves0 (car x))
                 (count-leaves0 (cdr x))))))

;;; A Tree is either:
;;; - a list of Trees
;;; - an Atom


;;; Tree -> Number
(define (count-leaves t)
  (cond
    [(list? t) (accumulate + 0 (map count-leaves t))]
    ;; else an Atom
    [else 1]))


(define ls '(1 3 (5 7) 9))
(equal? (count-leaves0 ls)
        (count-leaves ls))

;;; We can abstract out (accumulate + 0 ls) as sum:
(define (sum ls)
  (accumulate + 0 ls))

;;; Tree -> Number
(define (count-leaves2 t)
  (cond
    [(list? t) (sum (map count-leaves2 t))]
    ;; else an Atom
    [else 1]))
(equal? (count-leaves0 ls)
        (count-leaves2 ls))


;; The stub proposed in the book is more awkward:
;; (define (count-leaves t)
;;   (accumulate ⟨??⟩ ⟨??⟩ (map ⟨??⟩ ⟨??⟩)))
;;; By using the HTDP template, the function is much simpler (as seen above)

(define (count-leaves-sicp t)
  (accumulate + 0
              (map
               (lambda (x) (if (list? x)
                               (count-leaves-sicp x)
                               1))
               t)))

(equal? (count-leaves0 ls)
        (count-leaves-sicp ls))
