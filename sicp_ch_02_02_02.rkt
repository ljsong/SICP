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

(car '(((5 3) (4 6))))

(define (make-mobile left right)
  (list left right))    ; notice: (list 1 2) is equal to (cons 1 (cons 2 null))
                        ; so (cdr (cons 1 (cons 2 null))) is (2) and (cdr (cons 1 2)) is 2
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
    (cadr branch))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define (balance? mobile)
  (cond ((not (pair? mobile)) #t)
        ((let ((left (left-branch mobile))
              (right (right-branch mobile)))
           (and (balance? (branch-structure left))    ; 3 conditions, left torque is equal to right one
                (balance? (branch-structure right))   ; left, right sub mobile is balance
                (= (* (branch-length left)
                      (total-weight (branch-structure left)))
                   (* (branch-length right)
                      (total-weight (branch-structure right)))))) #t)
        (else #f)))

(define left-left-child (make-branch 2 9))
(define left-right-child (make-branch 6 3))
(define left-mobile (make-mobile left-left-child left-right-child))
(define left-child (make-branch 2 left-mobile))

(define right-left-child (make-branch 2 2))
(define right-right-child (make-branch 1 4))
(define right-mobile (make-mobile right-left-child right-right-child))
(define right-child (make-branch 4 right-mobile))

(define top-mobile (make-mobile left-child right-child))
(total-weight top-mobile)
(balance? top-mobile)

#|
(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
|#

(define (tree-map func items)
  (cond ((null? items) '())
        ((not (pair? items)) (func items))
        (else
         (cons (tree-map func (car items))
               (tree-map func (cdr items))))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (square-tree items)
  (tree-map (lambda (x) (* x x)) items))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (items) (append items (list (car s)))) rest)))))
#| 具体的做法是将s的第一个元素添加到由s剩余的元素生成的子集当中
例如：第一个子集是(),接着将3添加到由s剩余元素生成的子集即'()中
此时生成新子集'(3)，已经生成的子集为'(), '(3)，再将2添加到已经生成的子集中
生成新的子集'(2), '(2 3)，此时的子集有'(),'(2),'(3),'(2 3)，再将1添加到
已生成的子集中，此时完成所有子集的生成过程  |#

(subsets (list 1 2 3))
(subsets (list 1 2 (list 3 4) 5))
