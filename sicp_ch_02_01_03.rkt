#lang racket

;;;;;;;;;;;;;;;;;;;;;;;; SUB SECTION 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x) (* x x))

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car z)
  (if (= 0 (remainder z 2))
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (cdr (/ z 3)))
      0))

(define test (cons 4 5))
(car test)
(cdr test)

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define one-square (one square))
(define two-square (two square))
(define three-square ((add one two) square))

(one-square 2)
(two-square 2)
(three-square 2)