#lang racket

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (make-rat n d) 
  (let ((g (gcd (abs n) (abs d))))
    (if (< d 0) (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

(define (number x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+  (* (number x) (denom y))
                (* (denom x) (number y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (-  (* (number x) (denom y))
                (* (denom x) (number y)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (number x) (number y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (number x) (denom y))
            (* (denom x) (number y))))

(define (equal-rat? x y)
  (= (* (number x) (denom y))
     (* (number y) (denom x))))

(define (print-rat x)
  (display (number x))
  (display "/")
  (display (denom x))
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;; SUB SECTION 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (average x y)
  (/ (+ x y) 2))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point point)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")")
  (newline))

(define (make-segment start end)
  (cons start end))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (midpoint-segment line)
  (make-point (average (x-point (start-segment line))
                       (x-point (end-segment line)))
              (average (y-point (start-segment line))
                       (y-point (end-segment line)))))

(define start (make-point 2 5))
(define end (make-point 4 8))
(define line (make-segment start end))
(define mid (midpoint-segment line))
(print-point mid)

(define (make-rectangle lt rb)
  (cons lt rb))

(define (left-top rectangle)
  (car rectangle))

(define (right-bottom rectangle)
  (cdr rectangle))

(define (height rectangle)
  (abs (- (x-point (right-bottom rectangle))
          (x-point (left-top rectangle)))))

(define (width rectangle)
  (abs (- (y-point (left-top rectangle))
          (y-point (right-bottom rectangle)))))

(define (circumference rectangle)
  (* (+ (height rectangle)
        (width rectangle))
     2))

(define (area rectangle)
  (* (height rectangle)
     (width rectangle)))

(define rect (make-rectangle (make-point 1 1)
                             (make-point 3 4)))

(circumference rect)
(area rect)

; the other way is using height and width to represent a rectangle
; function height return the value of height directly and so does width