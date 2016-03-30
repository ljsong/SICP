#lang racket
(require scheme/mpair)

(define (mcddr pair)
  (mcdr (mcdr pair)))

(define (mcadr pair)
  (mcar (mcdr pair)))

(define (mcaar pair)
  (mcar (mcar pair)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))  ; (car records)是指向第一个元素的序对
        (else (assoc key (cdr records)))))           ; (caar records)是key, (cadr records)是值

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (list key-1
                                    (mcons key-2 value))
                              (mcdr local-table)))))
      'OK)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (make-bst-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key)
      (let ((records (mcdr local-table)))
        (define (inner-lookup key records)
          #| Of all the standard Scheme values, only #f counts as false in conditional expressions.
Except for #f, all standard Scheme values, including #t, pairs, the empty list, symbols,
numbers, strings, vectors, and procedures, count as true. |#
          (if (not (null? records)) ; do not use (if records) to expect false action when records ='()
              (let ((left-child (mcadr records))
                    (right-child (mcddr records))
                    (root-key (mcaar records)))
                (cond ((< key root-key) (inner-lookup key left-child))
                      ((> key root-key) (inner-lookup key right-child))
                      (else (mcar records))))
              #f))
        (inner-lookup key records)))
    
    (define (insert! key value)
      (let ((records (mcdr local-table)))
        (define (inner-insert! records key value)
          (let ((left-child (mcadr records))
                (right-child (mcddr records))
                (root-key (mcaar records)))
            (cond ((< key root-key)
                   (if (null? left-child)
                       (set-mcar! (mcdr records) (mcons (mcons key value) (mcons '() '())))
                       (inner-insert! left-child key value)))
                  ((> key root-key)
                   (if (null? right-child)
                       (set-mcdr! (mcdr records) (mcons (mcons key value) (mcons '() '())))
                       (inner-insert! right-child key value)))
                  (else
                   (set-mcdr! (mcar records) value)))))
        (if (null? (mcdr local-table))
            (set-mcdr! local-table (mcons (mcons key value) (mcons '() '())))
            (inner-insert! records key value))))
    
    (define (print)
      local-table)
    
    (define (dispatch m)
      (cond ((equal? m 'insert!) insert!)
            ((equal? m 'look-up) lookup)
            ((equal? m 'print) print)
            (else (error "Unknown command!" m))))
    dispatch))

(define t1 (make-bst-table))
(define get (t1 'look-up))
(define put (t1 'insert!))

(put 7 'a)
(put 9 'b)
(put 5 'c)
(put 3 'd)
(put 8 'e)
((t1 'print))
(get 4)