#lang racket

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'OK))

; [IN] a b c-in
; [OUT] sum c-out
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'OK))

; basic and or not
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'OK)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'OK)

(define (logical-and a b)
  (if (and (= a 1) (= b 1))
      1
      0))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'OK)

(define (logical-or a b)
  (if (or (= a 1) (= b 1))
      1
      0))

(define (or-gate-comb a1 a2 output)
  (let ((a1-inv (make-wire))
        (a2-inv (make-wire))
        (and-not-value (make-wire)))
    (inverter a1 a1-inv)
    (inverter a2 a2-inv)
    (and-gate a1-inv a2-inv and-not-value)
    (inverter and-not-value output))
  'OK)

; delay
(define (ripple-carry-adder A B S)
  (define (inner-add A B S c)
    (if (not (null? A))
        (begin
          (let ((c-out (make-wire)))
            (full-adder (mcar A) (mcar B) c (mcar S) c-out)
            (inner-add (mcdr A) (mcdr B) (mcdr S) c-out)))
        'Done))
  (let ((c (make-wire)))
    (set-signal! c 0)
    (inner-add A B S c)))

; implementation for wire
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'Done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'Done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; implementation for after-delay
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'Done
      (let ((first-item (first-agenda-item (the-agenda)))
            (first-item)
            (remove-first-agenda-item! the-agenda)
            (propagate)))))

(define (probe name wire)
  (add-action! wire
               (lambda()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (mcar s))
(define (setment-queue s) (mcdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time)
  (set-mcar! agenda time))

; test for simulator
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)
           