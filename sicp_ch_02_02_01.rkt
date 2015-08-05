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
      