#lang racket

(require "sicp_ch_02_04_02.rkt")
(require "sicp_ch_02_04_01.rkt")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put '(scheme-number scheme-number) 'add 
       (lambda (x y) (tag (+ x y))))
  (put '(scheme-number scheme-number) 'sub 
       (lambda (x y) (tag (- x y))))
  (put '(scheme-number scheme-number) 'mul 
       (lambda (x y) (tag (* x y))))
  (put '(scheme-number scheme-number) 'div 
       (lambda (x y) (tag (/ x y))))
  (put 'scheme-number 'make 
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'scheme-number 'make) n))

(define (install-rational-package)
  ;;internal procedures
  (define (number x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (add-rat x y)
    (make-rat (+ (* (number x) (denom y))
                 (* (number y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (number x) (denom y))
                 (* (number y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (number x) (number y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (number x) (denom y)
                 (number y) (denom x))))

  (define (tag x) (attach-tag 'rational x))
  (put '(rational rational) 'add 
       (lambda (x y) (tag (add-rat x y))))
  (put '(rational rational) 'sub 
       (lambda (x y) (tag (sub-rat x y))))
  (put '(rational rational) 'mul 
       (lambda (x y) (tag (div-rat x y))))
  (put '(rational rational) 'div 
       (lambda (x y) (tag (div-rat x y))))
  (put 'rational 'make 
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangle and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-real-imag (* (magnitude z1) (magnitude z2))
                         (* (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-real-imag (/ (magnitude z1) (magnitude z2))
                         (/ (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put '(complex complex) 'add
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put '(complex complex) 'sub
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put '(complex complex) 'mul
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put '(complex complex) 'div
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'complex 'make-from-real-imag 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'complex 'make-from-mag-ang 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put '(complex) 'magnitude magnitude)
  (put '(complex) 'real-part real-part)
  (put '(complex) 'imag-part imag-part)
  (put '(complex) 'angle angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'complex 'make-from-real-imag) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'complex 'make-from-mag-ang) r a))

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define z (make-complex-from-real-imag 3 4))
(magnitude z)