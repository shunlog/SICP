#lang racket/base

(require pict)


(define (tunnel n)
  (define r (rectangle 100 200))
  (if (= n 0)
      r
      (lt-superimpose
       r
       (inset (scale (tunnel (- n 1)) 0.85) 
              25 20 0 0))))

(tunnel 30)


(define (spiral n)
  (define sq (rectangle 200 200))
  (if (= n 0)
      sq
      (pin-over
       sq 0 0
       (rotate (scale (spiral (- n 1)) 0.95)
               0.1))))

(inset (spiral 80) 30)

