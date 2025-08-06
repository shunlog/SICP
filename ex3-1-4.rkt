#lang sicp

(define (check-expect actual expected)
  (if (equal? actual expected)
      (begin (display "ok")
             (newline))
      (begin
        (display "FAIL")
        (display (list 'expected: expected 'got: actual)))))


(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((bal2 100))
    (lambda (amount)
      (if (>= bal2 amount)
          (begin (set! bal2 
                       (- bal2 amount))
                 bal2)
          "Insufficient funds"))))

(new-withdraw 100)


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (get-balance) balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) get-balance)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  dispatch)

(define acc1-dispatch (make-account 100))
((acc1-dispatch 'deposit) 50)
((acc1-dispatch 'balance))
((acc1-dispatch 'withdraw) 25)
((acc1-dispatch 'balance))


;; Ex. 3.1 Accumulator

(define (make-accumulator initial)
  (lambda (value)
    (begin (set! initial (+ initial value))
           initial)))

(define accum (make-accumulator 10))
(check-expect (accum 10) 20)
(check-expect (accum 30) 50)


;; Ex. 3.2 Monitoring function calls

;; Func/1 -> Func/1
(define (make-monitored f)
  (let ((count 0))
    (define (exec arg)
     (begin (set! count (+ count 1))
            (f arg)))
    (define (reset-count)
      (set! count 0))
   (lambda (arg)
     (cond 
       ([eq? arg 'how-many-calls?] count)
       ([eq? arg 'reset-count] (reset-count))
       (else (exec arg))))))

;; (define s (make-monitored sqrt))
;; (s 100)                                 ; -> 10
;; (s 'how-many-calls?)                    ; -> 1


;;; Exercise 3.3 + 3.4 Password protected function


(define (make-protected-account balance pass0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (get-balance) balance)
  (define (dispatch m pass)
    (cond 
      ([not (eq? pass pass0)] "Incorrect password")
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      ((eq? m 'balance) get-balance)
      (else (error "Unknown request: 
                 make-account" m))))
  dispatch)


;;; Exercise 3.4

(define (make-protected-account2 balance pass0)
  ;; The let is like private variables inside the class
  (let ((incorrect-count 0))

    ;; These are like private methods that have access
    ;; to the private variables
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (get-balance) balance)

    (define (call-the-cops)
      (error "Cops are on their way!"))

    (define (check-password pass)
      (if (eq? pass pass0)
          (begin (set! incorrect-count 0)
                 #t)
          (begin (set! incorrect-count (+ 1 incorrect-count))
                 (if (< 3 incorrect-count)
                     (call-the-cops))
                 #f)))

    ;; Dispatch is like the mechanism for calling methods of the class
    ;; or in other words for "passing messages"
    (define (dispatch m)
      (cond 
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        ((eq? m 'balance) get-balance)
        (else (error "Unknown request: 
                 make-account" m))))  

    (define (dispatch-w-pass m pass)
      (if (not (check-password pass))
          (error "Incorrect password")
          (dispatch m)))
    
    dispatch-w-pass))
