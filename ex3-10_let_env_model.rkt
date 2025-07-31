;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex3-10_let_env_model) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
#lang sicp

;; Recall from 1.3.2 that let is simply syntactic sugar
;; for a procedure call:
;; (let ((⟨var⟩ ⟨exp⟩)) ⟨body⟩)
;; is interpreted as an alternate syntax for
;; ((lambda (⟨var⟩) ⟨body⟩) ⟨exp⟩)


(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))))

;;; Rewrite the "let"
(define (make-withdraw2 initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance 
                        (- balance amount))
                  balance)
           "Insufficient funds")))
   initial-amount))

;;; Instead of lambda use named func

(define (make-withdraw3 initial-amount)
  (define (curry-bal balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds")))
  (curry-bal initial-amount))

((make-withdraw3 100) 50)
