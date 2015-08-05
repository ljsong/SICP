#lang racket

(define (make-interval a b)
  (cons a b))

(define (upper-bound c)
  (cdr c))

(define (lower-bound c)
  (car c))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2)
                   (max p1 p2))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2)) 

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (/ p 100)))
    (make-interval (- c (/ p 100)) (+ c (/ p 100)))))

(define (percent i)
  (* (/ (- (upper-bound i) (lower-bound i)) 2) 100))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-interval 3 5))
(define r2 (make-interval 4 6))

(par1 r1 r2)
(par2 r1 r2)

(define r3 (make-interval 3.5 3.6))
(define r4 (make-interval 4.0 4.1))
(div-interval r3 r3)
(div-interval r3 r4)
    