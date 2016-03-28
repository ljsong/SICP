#lang racket
(require scheme/mpair)

(define (make-queue)
  (mcons '() '()))

(define (front-ptr queue)
  (mcar queue))
(define (rear-ptr queue)
  (mcdr queue))
(define (set-front-ptr! queue item)
  (set-mcar! queue item))
(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with empty queue" queue)
      (mcar (front-ptr queue))))

(define (mcddr item)
  (mcdr (mcdr item)))
(define (mcadr item)
  (mcar (mcdr item)))

(define (front-insert-deque! queue item)
  (let ((new-pair (mcons item (mcons '() '()))))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair))
        (begin
          (set-mcdr! (mcdr new-pair) (front-ptr queue))
          (set-mcar! (mcdr (front-ptr queue)) new-pair)   ;不要使用let创建局部变量并使用set!修改,
          (set-front-ptr! queue new-pair)))))             ;这修改的只是局部的变量，队列的指针并未被修改

(define (rear-insert-deque! queue item)    ;每一个队列元素包含三项:元素，前向指针，后向指针
  (let ((new-pair (mcons item (mcons '() '()))))
    (if (empty-queue? queue)
        (begin 
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair))
        (begin
          (set-mcdr! (mcdr (rear-ptr queue)) new-pair)
          (set-mcar! (mcdr new-pair) (rear-ptr queue))
          (set-rear-ptr! queue new-pair)))))

(define (front-delete-deque! queue)
  (if (empty-queue? queue)
      (error "DELETE called with empty queue" queue)
      (begin
        (set-front-ptr! queue (mcddr (front-ptr queue)))
        (set-mcar! (mcdr (front-ptr queue)) '()))))

(define (rear-delete-deque! queue)
  (if (empty-queue? queue)
      (error "DELETE called with empty queue" queue)
      (begin
        (set-rear-ptr! queue (mcadr (rear-ptr queue)))
        (set-mcdr! (mcdr (rear-ptr queue)) '()))))

(define (print-deque queue)
  (define (print-item item)
    (if (null? item)
        (display "")
        (begin
          (display (mcar item))
          (display " ")
          (print-item (mcddr item)))))
  (display "(")
  (print-item (front-ptr queue))
  (display ")\n"))

(define q1 (make-queue))

(rear-insert-deque! q1 'a)
(front-insert-deque! q1 'b)

(rear-insert-deque! q1 'c)
(front-insert-deque! q1 'd)
(print-deque q1)

(rear-delete-deque! q1)
(print-deque q1)

(front-delete-deque! q1)
(print-deque q1)