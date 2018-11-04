;; Unique pair of integers in a range 1 to n 
;; such that 1 <= j < i <= n

;; Enumerate a range of integers

(define (enumerate start end)
  (if (> start end)
      '()
      (cons start (enumerate (+ start 1) end))))

;; Accumulate

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;; flatmap is a combination of accumulate and map
;; Accumulate is applied to the result of map 

(define (flatmap proc sequence)
  (define nil '())
  (accumulate append nil (map proc sequence)))

;; Create a unique set of pairs (i, j) 
;; in the range 1 to n such that the following 
;; condition holds true: 1 <= j < i <= n

(define (unique-pairs n)
  (flatmap (lambda (i) 
	   (map (lambda (j) (list j i)) (enumerate 1 (- i 1))))
	   (enumerate 1 n)))

