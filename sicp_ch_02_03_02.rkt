#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(define a '(1 2 3 4))
(define b '(3 4 5 9))
(union-set a b)
(intersection-set a b)

(define (intersection-ordered-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-ordered-set (cdr set1)
                                               (cdr set2))))
              ((< x1 x2)
               (intersection-ordered-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-ordered-set (cdr set2) set1))))))

(define (element-of-ordered-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-ordered-set? x (cdr set)))))

(define (adjoin-ordered-set x s)
  (if (element-of-ordered-set? x s)
      s
      (cons x s)))

#| s1和s2交替前进，当(car s1) == (car s2)时两个列表同时前进一个元素，
 复杂度为O(Ls1 + Ls2) |#
(define (union-ordered-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1)) (x2 (car s2)))
           (cond ((< x1 x2) (cons x1
                                  (union-ordered-set
                                   (cdr s1)
                                   s2)))
                 ((> x1 x2) (cons x2 (union-ordered-set
                                 (cdr s2)
                                 s1)))
                 ((= x1 x2) (cons x1 (union-ordered-set
                                      (cdr s1)
                                      (cdr s2)))))))))
          

(define s '(1 2 3 4 7))
(adjoin-ordered-set 2 s)
(union-ordered-set a b)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set?
                            x
                            (left-branch set)))
        ((> x (entry set)) (element-of-set?
                            x
                            (right-branch set)))))

(define (adjoin-tree-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x ((left-branch set)))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x ((right-branch set)))))))

#| 初始认为平衡树为空树，remaining-elts为全部元素
 将列表分为三部分即，左子树 | 根 | 右子树 |#
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)   ;当长度为0时在list前插入空表表示树节点为NULL
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (union-tree-set s1 s2)
  

(list->tree '(1 3 5 7 9 11))
      