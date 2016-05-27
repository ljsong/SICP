#lang racket
(require racket/stream)

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (filter-stream pred stream)
  (cond ((stream-empty? stream) empty-stream)
        ((pred (stream-first stream))
         (cons (stream-rest stream)
               (stream-filter pred
                              (stream-rest stream))))
        (else (filter-stream pred (stream-rest stream)))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
          (set! already-run? true)
          result)
      result))))

(define (delay proc)
  (memo-proc (lambda() (proc))))

(define (force delayed-object)
  (delayed-object))

(define (map-stream proc . argstreams)
  (if (stream-empty? (stream-first argstreams))
      empty-stream
      (stream-cons
       (apply proc (map stream-first argstreams))
       (apply map-stream
              (cons proc (map stream-rest argstreams))))))

(define (show x)
  (displayln x)
  x)

(define x (map-stream show (stream-enumerate-interval 0 10)))
(stream-ref x 5)

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define (display-stream s)
  (stream-for-each displayln s))

(define seq (map-stream accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
  seq))

(stream-ref y 7)
(display-stream z)

(provide map-stream)