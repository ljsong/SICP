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