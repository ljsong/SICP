#lang racket

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (square x)
  (* x x))

(define (pfunc x y cx cy radius)
  (<= ( + (square (- x cx)) (square (- y cy))) radius))

(define (estimate-integral pfunc x1 x2 y1 y2 trials)
  (define cx (/ (+ x2 x1) 2))
  (define cy (/ (+ y2 y1) 2))
  (define (p)
    (let ((rx (random-in-range x1 x2))
          (ry (random-in-range y1 y2)))
      (pfunc rx ry cx cy 1)))
  (* (monte-carlo trials p) (- x2 x1) (- y2 y1)))

(estimate-integral pfunc 2 4 2 4 100)

(define f                          ; this define a id f that binding to the following expression
  (let ((bound 0))                 ; (define (f x y) ....) is equal to the following expression:
    (lambda (num)                  ; (define f (lambda (x y) (....))), so 
       (define ret 0)              ; (define (f) ....) is not equal to (define f ....)
       (if (< num bound)           ; the former one is equal to (define f (lambda () ....)))
           (set! ret 1)
           (set! ret 0))
       (set! bound num) ret)))

(+ (f 0) (f 1))
(+ (f 1) (f 0))