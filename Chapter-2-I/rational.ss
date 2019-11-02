;; Exercise 2.1
;; Define a datatype for rational numbers
;; This involes creating a constructor for creating rational numbers, a function for printing rational
;; numbers and a method for selected numerator and denominator from a rational number

(define (make-rat n d)
  (cond
   ((and (> 0 n) (< 0 d)) (cons (- n) d))
   ((and (< 0 n) (> 0 d)) (cons (- n) (abs d)))
   (else (cons n d))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
