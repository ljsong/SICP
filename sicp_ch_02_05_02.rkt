#lang racket
(require "sicp_ch_02_04_01.rkt")
(require "sicp_ch_02_04_02.rkt")
(require "sicp_ch_02_05_01.rkt")

#|
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args))
                    (type1 (car type-tags))
                    (type2 (cadr type-tags)))
                (if (equal? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))
|#

(define (install-polynomial-package)
  ;;install prodedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (variable? x) (symbol? x))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2)
                       (adjoin-term
                        t2 (add-terms L1) (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (add-terms L1 (neg L2)))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L)))))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (neg p)
    (let ((tlist (term-list p)))
      (let ((t1 (first-term tlist))
            (rlist (rest-terms tlist)))
        (let ((neg-term (make-term (- 0 (order t1)) (coeff t1))))
          (make-poly (variable p) (add-terms neg-term (neg rlist)))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same ver -- ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (=zero? p)
    (let ((tlist (term-list p)))
      (and (= (order (first-term tlist)) 0) (=zero? (rest-terms tlist)))))
   
  (define (tag p) (attach-tag 'polynomial p))
  (put '(polynomial polynomial) 'add
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put '(polynomial polynomial) 'mul
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'polynomial 'make
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'polynomial '=zero?
       (lambda (p) (tag (=zero? p))))
  (put 'polynomial 'neg
       (lambda (p) (tag (neg p))))
  'done)

(install-polynomial-package)
(define (make-polynomial var terms)
  ((get 'polynomial 'make) var terms))

(define (add-polynomial p1 p2)
  (apply-generic 'add p1 p2))

(define (neg p)
  (apply-generic 'neg p))

(define p (make-polynomial 'x '((2 1) (3 2) (63))))
p
(neg p)
(add-polynomial p (neg p))