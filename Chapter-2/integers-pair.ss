;; Define a pair of integers with the following property

;; The integer can be stored as a single single number 2^a * 3^b
;; given a, b are both non negative integers. The first part being event 
;; and the second being odd

(define (exp base n)
  (define (iter x result)
    (if (= x 0)
	result
	(iter (- x 1) (* base result))))
  (iter n 1))

