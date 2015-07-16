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

(define (filtered-accumulate combiner null-value filter? term a next b)
  (if (> a b)
      null-value
      (if (filter? a)
          (combiner (term a)
                    (filtered-accumulate combiner null-value filter? term (next a) next b))
          (combiner null-value
                    (filtered-accumulate combiner null-value filter? term (next a) next b)))))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (square x) (* x x))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((= (remainder n test-divisor) 0) test-divisor)
          (else (find-divisor n (+ 1 test-divisor)))))
  (if (< n 2)
      #f
      (= n (smallest-divisor n))))

(filtered-accumulate + 0 prime? identity 1 inc 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SUB SECTION 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x ((lambda (x) (* x x)) a))
       (* y b)
       (* a b))))

(f 5 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SUB SECTION 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search f neg-point pos-point)
  (let ((midpoint ((lambda (x y) (/ (+ x y) 2)) neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((> test-value 0)
                 (search f neg-point midpoint))
                ((< test-value 0)
                 (search f midpoint pos-point))
                (else
                 (midpoint)))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (< a-value 0) (> b-value 0))
           (search f a b))
          ((and (> a-value 0) (< b-value 0))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0 2.0)

(define (fixed-point f first-guess)
  (newline)
  (display "****** Start guessing ******")
  (newline)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.000001))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)