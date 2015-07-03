#lang racket

(define (factorial n)
  (factor-iter 1 1 n))

(define (factor-iter product counter n)
  (if (> counter n)
      product
      (factor-iter (* product counter)
                   (+ 1 counter)
                   n)))

(factorial 6)

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)

(A 2 4)

(A 3 3)

(define (fib n)
  (define (fib-iter a b counter)
    (if (> counter n) a
                      (fib-iter b 
                                (+ a b)
                                (+ counter 1))))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fib-iter 0 1 1))))

(fib 2)

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

(define (f_1_11 n)
  (cond ((< n 3) n)
        (else (+ (f_1_11 (- n 1))
                 (* 2 (f_1_11 (- n 2)))
                 (* 3 (f_1_11 (- n 3)))))))

(f_1_11 5)

(define (pascal_triangle row column)
  (if (or (= column 0) (= column row))
      1
      (+ (pascal_triangle (- row 1)
                       (- column 1))
         (pascal_triangle (- row 1)
                          column))))

(pascal_triangle 10 4)

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 0.5)

(define (slow-expt b n)
  (if (= n 0)
      1
      (* b (slow-expt b (- n 1)))))

(define (expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* product b))))
  (expt-iter b n 1))

(define (fast-expt b n)
  (if (= n 0) 1
      (if (even? n) (square (fast-expt b (/ n 2)))
          (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(slow-expt 2 32)
;(expt 3 32)
;(fast-expt 3 32)
 
(define (f_1_16 b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (* b b)
                                     (/ n 2)
                                     a))
          (else (fast-expt-iter b
                                (- n 1)
                                (* b a)))))
  (fast-expt-iter b n 1))

(f_1_16 2 5)

(define (f_1_17 a b)
  (define (fast-multi-iter a b product)
    (cond ((= b 0) product)
          ((even? b) (fast-multi-iter (double a)
                                      (halve b)
                                      product))
          (else (fast-multi-iter a
                                 (- b 1)
                                 (+ a product)))))
  (fast-multi-iter a b 0))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(f_1_17 6 7)

(define  (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 9 4)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 269)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (format-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((format-test n) (fast-prime? n (- times 1)))
        (else #f)))

(fast-prime? 269 5)
                        