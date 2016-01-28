#lang racket
(require "sicp_ch_02_03_01.rkt")
(require "sicp_ch_02_04_01.rkt")

; 可以将构造函数make-xxx以'make存入表中，这样可以根据运算符动态加载构造函数

(define (apply-generic op args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get type-tags op)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

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

(define (square x) (* x x))

(define (install-rectangular-package)
  ;;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;;internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;;interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-deriv-package)
(install-rectangular-package)
(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ (* 3 x) (** x 5)) 'x)

(provide put)
(provide get)
(provide apply-generic)
(provide magnitude)