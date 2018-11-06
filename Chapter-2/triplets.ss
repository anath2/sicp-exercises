;; For a given integers n and s, find triplets such that
;; 1) sum of these integers equals s 
;; 2) all of these integers are less than or equal to n

;; Let the pair be (i, j, k)
;; All of these integers can take values, from 1 to n.
;; The problem involves finding distinct values for each i, j and k 
;; such that each value is less than or equal to n and then
;; filtering the result such that the sum equals s

;; Enumerate 

(define (enumerate n)
  (define (iter start end)
    (if (> start end)
	'()
	(cons start (iter (+ start 1) end))))
  (iter 0 n))

;; Flat map 

(define (flatmap proc sequence)
  (define nil '())	
  (accumulate append nil (map proc sequence)))

;; Unque triplets

(define (unique-triplets n)	
  (flatmap (lambda (i)())
	   (enumerate 1 n)))

