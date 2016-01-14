#lang racket
(require "sicp_ch_02_03_01.rkt")

; 可以将构造函数make-xxx以'make存入表中，这样可以根据运算符动态加载构造函数

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put type op item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get type op)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (multiplicand p)  ;注意区分与sicp_ch_02_03_01的区别，此时的p中已经去除*
  (if (null? (cddr p))
      (cadr p)
      (cons '* (cdr p))))

(define (install-deriv-package)
  (define (sum-deriv exp var)
    (make-sum (deriv (car exp) var)
              (deriv (cadr exp) var)))
  (define (mul-deriv exp var)
    (make-sum
     (make-product (car exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (car exp) var)
                   (multiplicand exp))))

  (define (power-deriv exp var)
    (make-product
     (cadr exp)
     (make-product
      (make-exponentiation (car exp) (- (cadr exp) 1))
      (deriv (car exp) var))))

  (when (not (get 'deriv '+))
    (put 'deriv '+ sum-deriv))

  (when (not (get 'deriv '*))
    (put 'deriv '* mul-deriv))

  (when (not (get 'deriv '**))
    (put 'deriv '** power-deriv)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(install-deriv-package)
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ (* 3 x) (** x 5)) 'x)
