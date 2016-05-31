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

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

(define (all-pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-first s) x))
                 (stream-rest t))
     (stream-map (lambda (x) (list x (stream-first s)))
                   (stream-rest t)))
    (all-pairs (stream-rest s) (stream-rest t)))))

(define test (pairs integers integers))
(stream-ref test 10)

(define (triples s t u)
  (stream-cons
   (list (stream-first s) (stream-first t) (stream-first u))
   (interleave
    (stream-map (lambda (x) (cons (stream-first s) x))
                (stream-rest (pairs t u)))
    (triples (stream-rest s) (stream-rest t) (stream-rest u)))))

(define test1 (triples integers integers integers))
(stream-ref test1 3)

(define pytha-stream
  (stream-filter (lambda (x)
                   (let ((first (car x))
                         (second (cadr x))
                         (third (caddr x)))
                     (= (+ (square first) (square second))
                        (square third))))
                 test1))
(stream-ref pytha-stream 2)

(define (merge-weight s1 s2 cpw)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
                (s2car (stream-first s2)))
            (cond ((< (cpw s1car) (cpw s2car))
                   (stream-cons s1car (merge-weight (stream-rest s1) s2 cpw)))
                  ((< (cpw s2car) (cpw s1car))
                   (stream-cons s2car (merge-weight s1 (stream-rest s2) cpw)))
                  (else
                   (stream-cons s1car
                                (merge-weight (stream-rest s1) s2 cpw))))))))

(define (weighted-pairs s1 s2 cpw)
  (stream-cons
   (list (stream-first s1) (stream-first s2))
   (merge-weight
    (stream-map (lambda (x) (list (stream-first s1) x))
                (stream-rest s2))
    (weighted-pairs (stream-rest s1) (stream-rest s2) cpw)
    cpw)))

(define test2 (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))

(define stream235 (stream-filter (lambda (x) (or (= (remainder x 2) 0)
                                                 (= (remainder x 3) 0)
                                                 (= (remainder x 5) 0)))
                                 integers))

(define test3 (weighted-pairs stream235 stream235 (lambda (x)
                                                    (+ (* 2 (car x))
                                                       (* 3 (cadr x))
                                                       (* 5 (car x) (cadr x))))))
(stream-ref test3 10)

(define (stream-list-n s n)
  (define (stream-list s cur)
    (when (< cur n)
        (begin (displayln (stream-ref s cur))
               (stream-list s (+ cur 1)))))
  (stream-list s 0))        

(define ramanujan-stream
  (let ((cube-stream (weighted-pairs
                     integers integers
                     (lambda (item)
                       (let ((x (car item))
                             (y (cadr item)))
                         (+ (* x x x)
                            (* y y y)))))))
    (define (weight item)
      (let ((x (car item))
            (y (cadr item)))
        (+ (* x x x)
           (* y y y))))
    (define (ramanujan-num s)
      (let ((first (stream-first s))
            (second (stream-first (stream-rest s))))
      (if (= (weight first) (weight second))
          (stream-cons (cons (weight first) (list first second))
                       (ramanujan-num (stream-rest (stream-rest s))))
          (ramanujan-num (stream-rest s)))))
    (ramanujan-num cube-stream)))

(stream-list-n ramanujan-stream 5)

(define sum-of-square-stream
  (let ((square-stream (weighted-pairs
                        integers integers
                        (lambda (x) (+ (square (car x))
                                   (square (cadr x)))))))
    (define (weight item)
      (+ (square (car item))
         (square (cadr item))))
    (define (sum-of-square s)
      (let ((first (stream-first s))
            (second (stream-first (stream-rest s)))
            (third (stream-first (stream-rest (stream-rest s)))))
        (if (and (= (weight first) (weight second))
                 (= (weight second) (weight third)))
            (stream-cons (cons (weight first) (list first second third))
                         (sum-of-square (stream-rest (stream-rest (stream-rest s)))))
            (sum-of-square (stream-rest s)))))
    (sum-of-square square-stream)))

(stream-list-n sum-of-square-stream 20)