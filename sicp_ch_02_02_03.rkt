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
                      (else (+ 1 y)))) 0 sequence))
; 此时的x和y已经是(accumulate op initial sequence),注意根据op操作的不同
; (accumulate op initial sequence)的类型也不同，可能是一个数，也可能是一个序列等等

(length-test (list (list 2 3)))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))
;higher-terms 此时为(accumulate op initial (cdr sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (sub-tree)
                         (if (pair? sub-tree)
                             (count-leaves sub-tree)
                             1)) t)))

(count-leaves (list (list 1 2) 3 (list 4 (list 5))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op initial (map car seqs))
            (accumulate-n op initial (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)

(define (scale-list list factor)
  (cond ((null? list) null)
        ((not (pair? list)) (* list factor))
        (else (cons (scale-list (car list) factor)
                    (scale-list (cdr list) factor)))))

(scale-list '(1 2 3 (4 (5) 6) 7) 2)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (seq)
         (dot-product seq v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (seq)
           (map (lambda (col)
                  (dot-product seq col)) cols)) m)))

(define (matrix-*-matrix-1 m n)
  (let ((cols (transpose n)))
    (map (lambda (seq)
           (matrix-*-vector cols seq)) m)))

(define m '((1 2 3 4) (4 5 6 7) (7 8 9 0)))
(define n '(4 7 2 5))
(define v '((1 2 3) (4 5 6) (7 8 9)))
(define w '((5 4 7 1) (2 5 6 0) (3 2 8 9)))

(matrix-*-vector m n)

(transpose m)

(matrix-*-matrix v w)
(matrix-*-matrix-1 v w)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(accumulate * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))
(accumulate list null (list 1 2 3))
(fold-left list null (list 1 2 3))

(define (reverse sequence)
  (accumulate (lambda (x y) (append y (list x))) null sequence))

(define (reverse1 sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

(reverse1 (list 1 2 3))
(reverse (list 1 2 3 4 5 6))