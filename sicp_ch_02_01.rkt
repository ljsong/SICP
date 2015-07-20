#lang racket

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (make-rat n d) 
  (let ((g (gcd (abs n) (abs d))))
    (if (< d 0) (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

(define (number x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+  (* (number x) (denom y))
                (* (denom x) (number y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (-  (* (number x) (denom y))
                (* (denom x) (number y)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (number x) (number y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (number x) (denom y))
            (* (denom x) (number y))))

(define (equal-rat? x y)
  (= (* (number x) (denom y))
     (* (number y) (denom x))))

(define (print-rat x)
  (display (number x))
  (display "/")
  (display (denom x))
  (newline))