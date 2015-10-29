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

#|(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
|#

(define (unique-pairs n)
  (map (lambda (i)
         (map (lambda (j)
                (list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (accumulate
                           append null (unique-pairs n)))))

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

(unique-pairs 5)

(define (triple-sum n s)
  (define all-seqs (map (lambda (i)
         (map (lambda (j)
                     (if (and (not (= j i))
                                (>= (- s i j) 1)
                                (<= (- s i j) n)
                                (not (= (- s i j) i))
                                (not (= (- s i j) j)))
                       (list i j (- s i j))
                       null))
              (enumerate-interval 1 n)))
       (enumerate-interval 1 n)))
  (filter (lambda (seq)
            (not (null? seq)))
          (accumulate append null all-seqs)))
                     

(triple-sum 6 10)

(define empty-board null)

(define (safe? k positions)
  (define pos (car (reverse positions)))
  (null? (filter (lambda (seq) (or (= (car pos) (car seq))
                                     (= (abs (- (cadr pos) (car pos)))
                                        (abs (- (cadr seq) (car seq))))))
                 (cdr (reverse positions)))))

(define (queens board-size)
  (define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list (list new-row k))))
  
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  
  (queen-cols board-size))

(queens 8)