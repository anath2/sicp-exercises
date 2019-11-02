;; Create data type for line.
;; A line can be defined in terms of points on a plane.
;; With each point represented by a pair of integers

(define (make-point a b) (cons a b))

(define x-cord car)
(define y-cord cdr)

;; Define a line

(define (make-line a b) (cons a b))

(define start-segment car)

(define end-segment cdr)

;; Create a rectangle

