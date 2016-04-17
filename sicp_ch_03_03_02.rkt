#lang racket
(require scheme/mpair)

(provide make-queue)
(provide insert-queue!)
(provide delete-queue!)
(provide empty-queue?)
(provide front-queue)

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (if (empty-queue? queue)
        (begin
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
        (begin ;设置分为两步，第一步将new-pair链接到rear-ptr的后面，第二步更新rear-ptr
          (set-mcdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))

(define (delete-queue! queue)
  (if (empty-queue? queue)
      (error "DELETE! called with an empty queue" queue)
      (begin
        (set-front-ptr! queue (mcdr (front-ptr queue)))
        queue)))

(define (print-queue queue)
  (define (print-item item)
    (if (null? item)
        (display "")
        (begin
          (display (mcar item))
          (display " ")
          (print-item (mcdr item)))))
  (display "(")
  (print-item (front-ptr queue))
  (display ")\n"))

(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)


(define (make-queue-with-status)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    
    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (if (empty-queue?)
            (begin
              (set! front-ptr new-pair)
              (set! rear-ptr new-pair))
            (begin
              (set-mcdr! rear-ptr new-pair)
              (set! rear-ptr new-pair)))))
    
    (define (delete-queue!)
      (if (empty-queue?)
          (error "DELETE! called with an empty queue")
          (begin
            (set! front-ptr (mcdr front-ptr)))))
    
    (define (print-queue)
      (define (print-item item)
        (if (null? item)
            (display "")
            (begin
              (display (mcar item))
              (display " ")
              (print-item (mcdr item)))))
      (display "(")
      (print-item front-ptr)
      (display ")\n"))
    (define (dispatch m)
      (cond ((equal? m "insert") insert-queue!)
            ((equal? m "delete") delete-queue!)
            ((equal? m "print") print-queue)
            (else
             (error "Unsupported command"))))
    dispatch))

(define q2 (make-queue-with-status))
(define insert! (q2 "insert"))
(define delete! (q2 "delete"))
(define print (q2 "print"))
(insert! 'a)
(print)
(insert! 'b)
(print)
(delete!)
(print)
  