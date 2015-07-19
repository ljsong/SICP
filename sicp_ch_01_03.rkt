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
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.000001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)

(define (cont-frac n d k)
  (define (cont-frac-recur c)
    (let ((a (n c))
          (b (d c)))
      (if (= c k)
          (/ a b)
          (/ a (+ b (cont-frac-recur (+ c 1)))))))
  (define (cont-frac-iter c result)
    (let ((a (n c))
          (b (d c)))
      (if (= c 0)
          result
          (cont-frac-iter (- c 1) (/ a (+ b result))))))
  ;(cont-frac-recur 1)
  (cont-frac-iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(define (approximate-e k)
  (define (d k)
    (if (= (remainder k 3) 2)
        ((lambda (x) (* 2 (+ (quotient x 3) 1))) k)
        1))
  (cont-frac (lambda (i) 1.0)
             d
             k))

(approximate-e 50)

(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (* (- x) x)))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))

(tan-cf 3.1415926 6)


;;;;;;;;;;;;;;;;;;;;;; SUB SECTION 4 ;;;;;;;;;;;;;;;;;;;;
(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(sqrt 5)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(cube-root 9)

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-test x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(sqrt-test 14)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-damp x)
  (fixed-point-of-transform  (lambda (y) (/ x y))
                             average-damp
                             1.0))

(define (sqrt-newtons x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(sqrt-damp 5)
(sqrt-newtons 5)

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (ex_1_40 a b c)
  (newtons-method (cubic a b c) 1.0))

(ex_1_40 3 2 1)

(define (double g)
  (lambda (x) (g (g x))))

(((double (double double)) inc) 5)

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

(define (repeated f n)
  (define (repeated-iter f g n)
    (if (= n 0)
        g
        (repeated-iter f (compose f g) (- n 1))))
  (repeated-iter f (lambda (x) x) n))

((repeated square 3) 2)

(define (smooth f)
  (define dx 0.000001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

((smooth square) 2)

(define (to-integer number)
  (truncate (inexact->exact number)))

(define (power x n)
  (cond ((= n 0) 1)
        ((even? n) (power (square x) (/ n 2)))
        (else (* x (power x (- n 1))))))

(power 2 5)

(define (rootn x n)
  (define c (to-integer (/ (log n) (log 2))))
  (fixed-point ((repeated average-damp c)
                (lambda (y) (/ x (power y (- n 1)))))
                1.0))

(rootn 1000 3)

(define (iterative-improve good-enough? improve)
  (define (try-guess guess)
    (define next (improve guess))
    (if (good-enough? guess next)
        next
        (try-guess next)))
  try-guess)

(define (sqrt-ex x)
  ((iterative-improve (lambda (x y) (if (< (abs (- x y)) 0.00001) #t #f))
                     (lambda (y) (/ (+ y (/ x y)) 2)))
   1.0))

(sqrt-ex 16)

(define (fixed-point-ex f x)
  ((iterative-improve (lambda (x y) (if (< (abs (- x y)) 0.00001) #t #f))
                     (average-damp f))
   x))

(fixed-point-ex (lambda (y) (+ (sin y) (cos y))) 1.0)