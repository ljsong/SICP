#lang racket

(define (cube x)
  (* x x x))

; This is a recursive version
#|(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
|#

; iter version
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n)
  (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 100000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.001)

(define (even? x)
  (= (remainder x 2) 0))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term x)
    (define k (/ (- x a) h))
    (cond ((or (= k 0) (= k n)) (f x))
          ((even? k) (* 2 (f x)))
          (else (* 4 (f x)))))
  (define (simpson-next x)
    (+ x h))
    
  (* (/ h 3) (sum simpson-term a simpson-next b)))

(simpson-integral cube 0 1 100)

#| recursive version
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
|#

; iter version
(define (product term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (* (term x) result))))
  (iter 1 1))

(define (factorial n)
  (product identity 1 inc n))

(factorial 5)

(define (ex_1_31 n)
  (define (term x)
    (* (/ (* 2 x) (+ (* 2 x) 1))
       (/ (* 2 (+ x 1)) (+ (* 2 x) 1))))
  (define (next x)
    (+ x 1.0))
  (* 4 (product term 1 next n)))

(ex_1_31 100)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-test term a next b)
  (accumulate + 0 term a next b))

(define (product-test term a next b)
  (accumulate * 1 term a next b))

(sum-test cube 1 inc 10)
(product-test identity 1 inc 5)
  