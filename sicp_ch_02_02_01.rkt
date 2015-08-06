#lang racket

(cons (cons 1
            (cons 2 3))
      4)

(cons (cons 1 2)
      (cons 3 4))

(define one-through-four (list 1 2 3 4))

(car one-through-four)
(cdr one-through-four)
(cons 10 one-through-four)
(cons one-through-four 5)
(cons one-through-four '(5 6 7))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25 36))
(list-ref squares 5)

(define (length items)
  (define (length-iter items n)
    (if (null? items)
        n
        (length-iter (cdr items) (+ n 1))))
  (length-iter items 0))

(length squares)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append one-through-four squares)

(define (last-pair items)
  (if (= (length items) 1)
      items
      (last-pair (cdr items))))

(last-pair (list 23 72 149 34))

(define (reverse items)
  (if (= (length items) 1)
      (car items)
      (cons (reverse (cdr items)) (car items))))

(reverse (list 1 4 9 16 25 36))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (= (length coin-values) 0))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

(define (even? number)
  (= (remainder number 2) 0))

(define (same-parity . w)
  (define first-number (car w))
  (define (same-parity-iter items)
    (cond ((null? items) '())
          ((even? (+ first-number (car items)))
           (cons (car items) (same-parity-iter (cdr items))))
          (else
           (same-parity-iter (cdr items)))))
  (same-parity-iter w))

(same-parity 0 1 2 3 4 5 6 7)

(define (map items func)
  (if (null? items)
      '()
      (cons (func (car items))
            (map (cdr items) func))))

(map (list 1 2 -3 4 -5 6) abs)

(define (scale-list items factor)
  (map items (lambda (x) (* x factor))))

(scale-list (list 1 2 3 4 5) 2)

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        (reverse answer)
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(square-list1 (list 1 2 3 4))