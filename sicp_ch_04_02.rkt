#lang racket
(require scheme/mpair)
(require "sicp_ch_04_01_01.rkt")
(require "sicp_ch_04_01_03.rkt")
(require "sicp_ch_04_01_04.rkt")

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-mcar! obj 'evaluated-thunk)
           (set-mcar! (mcdr obj) result)
           (set-mcdr! (mcdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)   ; 此处eval将lambda转为procedure
    env)
  'OK)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'OK)

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure) ; param: name
           (list-of-delayed-args arguments env) ; argus: vals of params
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (lazy-list items)
  (define (wrap-lazy-list first-item rest-items)
    (if (null? rest-items)
        (make-lambda '(m) (list first-item))
        (make-lambda '(m) (list first-item (wrap-lazy-list (car rest-items) (cdr rest-items))))))
  (wrap-lazy-list (car items) (cdr items)))

(define (eval exp env)
  (display exp)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (eval (lazy-list (text-of-quotation exp)) env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (mcons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (delay-it exp env)
  (mlist 'thunk exp env))

(define (tagged-mlist? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))

(define (thunk? obj)
  (tagged-mlist? obj 'thunk))

(define (mcadr obj)
  (mcar (mcdr obj)))

(define (mcaddr obj)
  (mcar (mcdr (mcdr obj))))

(define (thunk-exp thunk) (mcadr thunk))
(define (thunk-env thunk) (mcaddr thunk))

(define (evaluated-thunk? obj)
  (tagged-mlist? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (mcadr evaluated-thunk))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define the-global-environment (setup-environment))
(driver-loop)