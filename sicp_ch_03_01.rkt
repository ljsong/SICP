#lang racket

(define withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd m)
    (if (eqv? passwd pwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password for" m)))
  dispatch)

(define acc (make-account 100 "test"))
((acc "test" 'withdraw) 50)
((acc "test" 'withdraw) 60)
((acc "test" 'deposit) 40)

(define (make-accumulator init)
  (lambda (num)
    (begin (set! init (+ init num))
           init)))

(define A (make-accumulator 5))
(A 10)
(A 10)

(define (make-monitored func)
  (define call-count 0)
  (define (mf msg)
    (cond ((eq? msg 'how-many-calls?) call-count)
          ((eq? msg 'reset-count) (set! call-count 0))
          (else (set! call-count (+ call-count 1))
                (func msg))))
  mf)

(define s (make-monitored sqrt))
(s 100)
(s 25)
(s 36)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
  