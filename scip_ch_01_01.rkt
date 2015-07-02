#lang racket
(define (max_of_two a b) (if (> a b) a b))

(define (max_sum_of_three a b c) (cond ((> a b) (+ a (max_of_two b c)))
                                       ((> b c) (+ b (max_of_two a c)))
                                       ((> c a) (+ c (max_of_two a b)))))

(max_sum_of_three 5 8 1)

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (sqrt x)
  (define (good-enough? old_guess guess)
  (< (/ (abs (- guess old_guess)) guess) 0.0001))
  
  (define (improve guess)
  (average guess (/ x guess)))
  
  (define (sqrt-iter guess)
  (if (good-enough? guess (improve guess))
      guess
      (sqrt-iter (improve guess))))
  
  (sqrt-iter 1.0))

(sqrt 100000000)

(define (cube x)
  (define (good-enough? old_guess guess)
  (< (/ (abs (- guess old_guess)) guess) 0.0001))
  
  (define (improve guess)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  
  (define (cube-iter guess)
  (if (good-enough? guess (improve guess))
      guess
      (cube-iter (improve guess))))
  
  (cube-iter 1.0))


(cube 27)


