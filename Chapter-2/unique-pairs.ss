;; Unique pair of integers in a range 1 to n 
;; such that 1 <= j < i <= n

;; Enumerate a range of integers

(define (enumerate n)
  (define nil '())
  (define (inner a b)
    (if (b < a)
	nil
	(cons (a (inner (+ a 1) b)))))
  (inner 1 n))



