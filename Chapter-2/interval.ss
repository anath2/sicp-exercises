;; Define an interval constructor and define various operations on it
;; Including : 
;;    - Addition
;;    - Multiplication
;;    - Division

;; Interval constructor

(define make-interval cons)

;; Lower bound

(define lower-bound car)

;; Upper bound

(define upper-bound cdr)

;; Addition

(define (add-interval x y)
  (make-interval 
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

;; Multiplication

(define (multiply-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
  (make-interval 
    (min p1 p2 p3 p4)
    (max p1 p2 p3 p4))))

;; Division

(define (divide-interval x y)
  (multiply-interval x (make-interval (/ 1.0 (upper-bound y))
				      (/ 1.0 (lower-bound y)))))

  
