#lang racket
(require scheme/mpair)

(define (count-pairs x)
  (length (inner x '())))

(define (inner x memo-list)
  (if (and (pair? x)
           (false? (memq x memo-list)))
      (inner (car x)
             (inner (cdr x)
                    (cons x memo-list)))
      memo-list))

(count-pairs (cons (cons 1 2) (cons 3 4)))
(count-pairs '((((())))))
(count-pairs '(1 2 3))

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define (check-cycle x)
  (define (traversal x memo-list)
    (cond ((null? x) #f)
          ((mmemq x memo-list) #t)
          (else
           (traversal (mcdr x) (mcons x memo-list)))))
  (traversal x '()))

(define x (mlist 'a 'b))
(define z (make-cycle (mlist 'a 'b '(c d))))
(check-cycle z)

(define (quick-check-cycle x)
  (define (mcddr x)
    (mcdr (mcdr x)))
  (define (check-equal slow fast)
    (cond ((null? (mcdr slow)) #f)
          ((null? (mcddr fast)) #f)
          ((eq? slow fast) #t)
          (else
           (check-equal (mcdr slow) (mcddr fast)))))
  (check-equal x x))

(quick-check-cycle z)
        