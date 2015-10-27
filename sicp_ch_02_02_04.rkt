#lang racket

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next-divisor n)
  (if (= n 2)
      3
      (+ n 2)))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (if (not (pair? pair))
      #f
      (prime? (+ (car pair) (cadr pair)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (enumerate-interval m n)
  (if (> m n)
      null
      (append (list m) (enumerate-interval (+ m 1) n))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (test x)
 (map (lambda (i)
   (map (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))))
 (enumerate-interval 1 x)))

(test 6)

(prime-sum-pairs 7)

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
          seq))

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations (list 1 2 3 4))