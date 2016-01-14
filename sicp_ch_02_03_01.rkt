#lang racket

(define a 1)
(define b 2)
(list a b)
(list 'a 'b)

(car '(a b c))
(cdr '(a b c))

(define (memq symbol seq)
  (cond ((null? seq) #f)
        ((eq? symbol (car seq)) symbol)
        (else
         (memq symbol (cdr seq)))))

(memq 'apple '(apple banana pear))

(define (equal? seq1 seq2)
  (cond ((and (not (pair? seq1)) (not (pair? seq2)))
         (eq? seq1 seq2))
        ((and (pair? seq1) (pair? seq2))
         (and (equal? (car seq1) (car seq2))
              (equal? (cdr seq1) (cdr seq2))))
        (else #f)))

(equal? '() '(a))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation base exp)
  (cond ((eq? base 0) 0)
        ((eq? base 1) 1)
        ((eq? exp 0) 1)
        ((eq? exp 1) base)
        (else (list '** base exp))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp) (- (exponent exp) 1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* x y (+ x 3)) 'x)
(deriv '(+ (* 3 x) (** x 5)) 'x)

(provide make-sum)
(provide addend)
(provide augend)

(provide make-product)
(provide multiplier)

(provide make-exponentiation)
(provide exponent)
(provide base)

(provide variable?)
(provide same-variable?)