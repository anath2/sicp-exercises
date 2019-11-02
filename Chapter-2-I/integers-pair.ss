;; Define a pair of integers with the following property

;; The integer can be stored as a single single number 2^a * 3^b
;; given a, b are both non negative integers. The first part being even 
;; and the second being odd

;; Exponential

(define (exp base n)
  (define (iter x result)
    ;; Define base case
    (if (= 0 x)
	result
	(iter (- x 1) (* base result))))
  (iter n 1))

;; Count zero remainder eivision revisions

(define (count-zero-remainder-divisions n divisor count)
  (if (= 0 (remainder n divisor))
      (count-zero-remainder-divisions (/ n divisor) divisor (+ 1 count))
      count))


; Define cons cdr car

(define (my-cons a b) (* (exp 2 a) (exp 3 b)))
(define (my-car z) (count-zero-remainder-divisions z 2 0))
(define (my-cdr z) (count-zero-remainder-divisions z 3 0))





