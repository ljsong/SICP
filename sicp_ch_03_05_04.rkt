#lang racket

(require racket/stream)
(require "sicp_ch_03_05_02.rkt")

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

(define (integral-iter integrand initial-value dt)
  (stream-cons initial-value
               (if (stream-empty? integrand)
                   empty-stream
                   (integral-iter (stream-rest integrand)
                             (+ (* dt (stream-first (force integrand)))
                                   initial-value)
                             dt))))

(define (solve-iter f y0 dt)
  (solve f y0 dt))

(stream-ref (solve-iter (lambda (y) y) 1 0.001) 1000)

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

(define (RLC r l c dt)
  (lambda (vc0 il0)
    (define il (integral (delay dil) il0 dt))
    (define vc (integral (delay dvc) vc0 dt))
    (define dil (add-streams (scale-stream il (/ (- 0 r) l))
                             (scale-stream vc (/ 1 l))))
    (define dvc (scale-stream il (/ -1 c)))
    (map-stream cons vc il)))

(define (display-stream-n s n)
  (define (display-stream cnt)
    (when (< cnt n)
      (begin (displayln (stream-ref s cnt))
             (display-stream (+ cnt 1)))))
  (display-stream 0))

(define s1 ((RLC 1 1 0.2 0.1) 10 0))
(display-stream-n s1 20)