#lang racket

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (cond ((null? sequence) initial)
        ((not (pair? sequence)) sequence)
        (else
         (op (accumulate op initial (car sequence))
             (accumulate op initial (cdr sequence))))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(filter odd? (list 1 2 3 4 5))

(accumulate * 1 (list 1 2 3 4 5))
(accumulate * 1 (list (list 1 2) 3 4 5))

(define (fib n)
  (define (fib-iter a b counter)
    (if (= counter 0)
        a
        (fib-iter b (+ a b) (- counter 1))))
  (fib-iter 0 1 n))

(define (list-fib-squares n)
  (accumulate cons
              null
              (map (lambda (x) (* x x))
                   (map fib
                        (enumerate-interval 0 n)))))
(list-fib-squares 10)

(define (map-test p sequence)
  (accumulate (lambda (x y) 
                (cond ((null? x) y)
                      ((not (pair? x)) (cons (p x) y))
                      (else
                       (cons x y)))) null sequence))

(map-test (lambda (x) (* x x)) (list (list 1 2) 3 4 (list 5 6)))
(map-test (lambda (x) (* x x)) (list 1 2))

(define (append-test seq1 seq2)
  (accumulate cons seq2 seq1))

(append-test (list 1 2 3 4) '())
(append (list 1 2 3 4) (list 5 6 7 8))

(define (length-test sequence)
  (accumulate (lambda (x y) 
                (cond ((null? x) y)
                      ((not (pair? x)) (+ 1  y))
                      (else
                       (+ x
                          y)))) 0 sequence))
; 此时的x和y已经是(accumulate op initial sequence)

(length-test (list 1 (list 2 (list 3)) (list 4 5)))