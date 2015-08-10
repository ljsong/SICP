#lang racket

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items))
                 (count-leaves (cdr items))))))

(define x (cons (list 1 2) (list 3 4)))
(length x)
(count-leaves x)
(count-leaves (list 1 (list 2 (list 3 4))))

(define y (list (list 1 2) (list 3 4)))
y

#|
(define (reverse items)
  (define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (car remained-items) result))))
  (iter items '()))
|#

(define (reverse items)
  (if (null? items)
      '()
      (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
  (if (not (pair? items))
      items
      (append (deep-reverse (cdr items))
            (list (deep-reverse (car items))))))

(reverse y)
(deep-reverse y)

(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else
         (append (fringe (car tree))
                 (fringe (cdr tree))))))
                        
(fringe y)

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (newline)
  (display "left branch: ")
  (display mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
    (cdr branch))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define (balance? mobile)
  (define left (left-branch mobile))
  (define right (right-branch mobile))
  (if (not (pair? mobile)) #t
      (= (* (branch-length left) (total-weight left))
         (* (branch-length right) (total-weight right)))))

(define left-left-child (make-branch 5 3))
(define left-right-child (make-branch 4 6))
(define left-mobile (make-mobile left-left-child left-right-child))
(define left-child (make-branch 2 left-mobile))

(define right-left-child (make-branch 2 3))
(define right-right-child (make-branch 1 4))
(define right-mobile (make-mobile right-left-child right-right-child))
(define right-child (make-branch 4 right-mobile))

(define top-mobile (make-mobile left-child right-child))
(total-weight top-mobile)

