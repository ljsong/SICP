#lang racket
(require "sicp_ch_02_04_02.rkt")
(require "sicp_ch_04_01_03.rkt")

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; left to right
(define (list-of-values-ltr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env))
            (right (list-of-values (rest-operands exps) env)))
        (cons left right))))

; right to left
(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
      '()
      (let ((left (list-of-values (rest-operands exps) env))
            (right (eval (first-operand exps) env)))
        (cons left right))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

;(define (apply procedure arguments)
;  (cond ((primitive-procedure? procedure)
;         (apply-primitive-procedure procedure arguments))
;        ((compund-procedure? procedure)
;         (eval-sequence
;          (procedure-body procedure)
;          (extend-environment
;           (procedure-parameters procedure)
;           arguments
;           (procedure-environment procedure))))
;        (else
;         error "Unknown procedure type -- APPLY" procedure)))

; 'a = (quote a)
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; set! operatoins
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; definition operations
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caddr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

; lambda opreations
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; if operations
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin operations
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; functions
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond operations
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((predicates (cond-predicate first))
                  (symbol (cadr first)))
              (if (eq? symbol '=>)
                  (make-if predicates
                           ((list (caddr first)) predicates)
                           (expand-clauses rest))
                  (make-if (cond-predicate first)
                           (sequence->exp (cond-actions first))
                           (expand-clauses rest))))))))

; boolean operation
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

; logical and 
(define (eval-and exp env)
  (let ((and-clauses (cdr exp)))
    (if (last-exp? and-clauses)
        (eval (car and-clauses))
        (if (eval (car and-clauses))
            (eval-and (cdr exp))
            #f))))

; logical or
(define (eval-or exp env)
  (let ((or-clauses (cdr exp)))
    (if (last-exp? or-clauses)
        (eval (car or-clauses))
        (if (eval (car or-clauses))
            #t
            (eval-or (cdr exp))))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;((quoted? exp) (text-of-quotation exp))
        ;((assignment? exp) (eval-assignment exp env))
        ;((definition? exp) (eval-definition exp env))
        ;((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ;((lambda? exp)
         ;(make-procedure (lambda-parameters exp)
                         ;(lambda-body exp)
                         ;env))
        ;((begin? exp)
         ;(eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(put 'op 'quote (lambda (exp env) (text-of-quotation exp)))
;(put 'op 'set! eval-assignment)
;(put 'op 'define eval-definition)
;(put 'op 'if eval-if)
;(put 'op 'lambda (lambda (exp env)
;                   (make-procedure (lambda-parameters exp)
;                                   (lambda-body exp)
;                                   env)))
;(put 'op 'begin (lambda (exp env)
;                  (eval-sequence (begin-actions exp) env)))
(put 'op 'cond (lambda (exp env)
                 (eval (cond->if exp) env)))

(put 'op 'and eval-and)
(put 'op 'or eval-or)

(define (evaln exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'op (car exp)) ((get 'op (car exp)) exp env))
        ((application? exp)
         (apply (evaln (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type --EVAL" exp))))

(define (and->if exp)
  (expand-and-predicates (cdr exp)))

(define (expand-and-predicates predicates)
  (if (null? predicates)
      #f
      (make-if (car predicates)
               (expand-and-predicates (cdr predicates))
               #f)))

(define (or->if exp)
  (expand-or-predicates (cdr exp)))

(define (expand-or-predicates predicates)
  (if (null? predicates)
      #f
      (make-if (car predicates)
               #t
               (expand-or-predicates (cdr predicates)))))

(define (let->combination exp env)
  #|
  (define params '())
  (define values '())
  (define (extract-param-value param-values)
    (if (null? param-values)
        'Done
        (begin (append params (car (car param-values)))
               (append values (cadr (car param-values)))
               (extract-param-value (cdr param-values)))))
  |#
  (define params (map car (cadr exp)))
  (define values (map cadr (cadr exp)))
  (begin ;(extract-param-value (cadr exp))
         ((make-lambda params (caddr exp)) values)))

(define (let*->nested-lets exp env)
  (define let-assignments (cadr exp))
  (define (extract-let assignments)
    (if (last-exp? assignments)
        (list 'let (list (car assignments)) (caddr exp))
      (list 'let (list (car assignments)) (extract-let (cdr assignments)))))
  (extract-let let-assignments))

(define test '(let* ((x 3)
                    (y (+ x 2))
                    (z (+ x y 5)))
               (* x z)))
(let*->nested-lets test '())