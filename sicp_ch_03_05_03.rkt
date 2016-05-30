#lang racket
(require racket/stream)
(require "sicp_ch_03_05_02.rkt")

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(stream-ref (sqrt-stream 2) 8)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (partial-sums s)
  (define ps (stream-cons (stream-first s)
                          (add-streams ps (stream-rest s))))
  ps)

(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
(stream-ref pi-stream 9)

(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first
              (make-tableau transform s)))

(stream-ref (accelerated-sequence euler-transform pi-stream) 8)

#|(define (stream-limit s tolenrance)
  (let ((first (stream-first s))
        (second (stream-first (stream-rest s))))
    (if (< (abs (- first second)) tolenrance)
        second
        (stream-limit (stream-rest s) tolenrance))))
|#

(define (stream-limit s tolenrance)
  (define (stream-limit-inner first s tolenrance)
    (let ((second (stream-first s)))
      (if (< (abs (- first second)) tolenrance)
          second
          (stream-limit-inner second (stream-rest s) tolenrance))))
  (stream-limit-inner (stream-first s) (stream-rest s) tolenrance))

(define (sqrt x tolenrance)
  (stream-limit (sqrt-stream x) tolenrance))

(sqrt 2 0.00001)

(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))
(stream-ref ln2-stream 20)

(stream-ref (accelerated-sequence euler-transform ln2-stream) 8)
