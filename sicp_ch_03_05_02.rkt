#lang racket
(require racket/stream)
(require "sicp_ch_03_05_01.rkt")

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))
(stream-ref integers 8)

(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (sieve stream)
  (stream-cons
   (stream-first stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-first stream))))
           (stream-rest stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (stream-cons 1 (scale-stream double 2)))

(define (square x)
  (* x x))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-first ps)) n) true)
          ((divisible? n (stream-first ps)) false)
          (else (iter (stream-rest ps)))))
  (iter primes))

(define (add-streams s1 s2)
  (map-stream + s1 s2))

(define s1 (stream-cons 1 s1))
(define s2 (stream-cons 2 s2))
(define s3 (add-streams s1 s2))
(stream-ref s3 6)

#|
(define fac
  (let ((last-result -1))
    (if (not (= last-result -1))
        last-result
        (lambda (x)
          (if (= x 1)
              1
              (begin (set! last-result (* x (fac (- x 1))))
                     last-result))))))
|#

(define (mul-streams s1 s2)
  (map-stream * s1 s2))

(define factorials (stream-cons 1 (mul-streams factorials (stream-rest integers))))

(stream-ref factorials 7)

(define (partial-sums s)
  (let ((ps (stream-cons (stream-first s)
               (add-streams ps (stream-rest s)))))
    ps))
(define ps (partial-sums integers))
(stream-ref ps 9)

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons s1car
                               (merge (stream-rest s1)
                                      (stream-rest s2)))))))))

(define S (stream-cons 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
(stream-ref S 10)

(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


(define (div-streams s1 s2)
  (map-stream / s1 s2))

(define ones (stream-cons 1 ones))

(define (integrate-series s)
  (mul-streams s
               (div-streams ones integers)))

(define is (integrate-series integers))
(stream-ref is 7)

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))
(stream-ref exp-series 6)

(define cosine-series
  (stream-cons 1 (integrate-series (stream-map (lambda (x) (- 0 x)) sine-series))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(stream-ref cosine-series 4)
(stream-ref sine-series 5)

(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (add-streams (scale-stream (stream-rest s2) (stream-first s1))
                             (mul-series (stream-rest s1) s2)))) 

(define (reciprocal-series s)    ; constant item should be one
  (stream-cons 1 (scale-stream (mul-series (stream-rest s) (reciprocal-series s)) -1)))

(define (div-series s1 s2)
  (if (= (stream-first s2) 0)
      (error "Divisor should have constant item!")
      (scale-stream 
       (mul-series s1 (reciprocal-series
                       (scale-stream s2 (/ 1 (stream-first s2)))))
       (/ 1 (stream-first s2)))))
                   

(define tan-series (div-series sine-series cosine-series))
(stream-ref tan-series 9)

(provide add-streams)
(provide scale-stream)