#lang racket

(define (make-local-variable var val other-operations)
  (list (list 'let (list (list var '*unassigned*)) (list 'set! var val) other-operations)))

(define (scan-out-defines procedure-body)
  (cond ((null? procedure-body) '())
        (else
         (let ((first-statement (car procedure-body)))
           (if (pair? first-statement)
               (let ((first-operator (car first-statement))
                     (rest-operators (cdr first-statement)))
                 (if (equal? first-operator 'define)
                     (when (not (pair? (car rest-operators)))
                       (let ((var (car rest-operators))
                             (val (cadr rest-operators)))
                         (make-local-variable var val
                                              (scan-out-defines (cdr procedure-body)))))
                     (scan-out-defines (cdr procedure-body))))
               first-statement)))))

#|
(define (definition-variable exp)
  (cadr exp))

(define (definition-value exp)
  (caddr exp))

(define (definition? exp)
  (if (pair? exp)
      (eq? (car exp) 'define)
      (eq? exp 'define)))

(define (make-begin seq) (cons 'begin seq))

(define (scan-out-defines body) 
  (define (name-unassigned defines) 
    (map (lambda (x) (list (definition-variable x) '*unassigned*)) defines)) 
  (define (set-values defines) 
    (map (lambda (x)  
           (list 'set! (definition-variable x) (definition-value x)))  
         defines)) 
  (define (defines->let exprs defines not-defines) 
    (cond ((null? exprs)  
           (if (null? defines) 
               body 
               (list (list 'let (name-unassigned defines)  
                           (make-begin (append (set-values defines)  
                                               (reverse not-defines))))))) 
          ((definition? (car exprs)) 
           (defines->let (cdr exprs) (cons (car exprs) defines) not-defines)) 
          (else (defines->let (cdr exprs) defines (cons (car exprs) not-defines))))) 
  (defines->let body '() '()))
|#
          
(scan-out-defines '((define y (integral (delay dy) y0 dt)) (define dy (stream-map f y)) y))

(define (letrec-params exps)
  (cadr exps))

(define (letrec-body exps)
  (caddr exps))

(define (name-undefined params)
  (if (null? params)
      '()
      (map (lambda (param) (list (car param) '*undefined*)) params)))

(define (set-variables params)
  (if (null? params)
      '()
      (car (map (lambda (param) (cons 'set! param)) params))))

(define (letrec->let exps)
  (cond ((null? exps) '())
        (else
         (list 'let (name-undefined (letrec-params exps))
               (set-variables (letrec-params exps))
               (letrec-body exps)))))

(define letrec-test '(letrec ((fact
                               (lambda (n)
                                 (if (= n 1)
                                     1
                                     (* n (fact (- n 1)))))))
                              (fact 10)))
(letrec->let letrec-test)

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1))))))) 10)

((lambda (n)
   ((lambda (fact)          ; Y combinator
     (fact fact n))
    (lambda (ft k)
      (cond ((= k 0) 1)
            ((= k 1) 1)
            (else
             (+ (ft ft (- k 1)) (ft ft (- k 2)))))))) 10)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))         ; (even? even? odd? x)  ---> (even? even?) ---> Y combinator, odd? x ---> f(x)
   (lambda (ev? od? n)             ; = even?
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)             ; = odd?
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 10)