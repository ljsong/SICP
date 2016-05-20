#lang racket
(require racket/stream)

(define (stream-cons a b)
  (cons a (delay b)))

(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons (stream-car stream)
               (stream-filter pred
                              (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

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