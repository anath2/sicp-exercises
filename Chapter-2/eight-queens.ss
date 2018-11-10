;; Eight queen problem: Find all ways to arrange eight queens on the 
;; chess board such that no two queen cancel each other
;; Each queen is represented as a list of two elements: row and col
;; therefore, positions for n queen are represented by: a list of lists :
;; ((list row col), (list row col), ...)

;; Constants

(define empty-board '())	
(define nil '())	

;; Accumulate

(define (accumulate op init sequence)
  (if (null? sequence)
      nil
      (op (car sequence)
	  (accumulate op init (cdr sequence)))))

;; Enumerate

(define (enumerate-range start end)
  (if (> start end) 
      nil
      (cons start (enumerate-range (+ 1 start) end))))

;; FlatMap

(define (flatmap proc sequence)
  accumulate append nil (map proc sequence))  

;; Safe: 
;; Checks if a position is safe to place the queen or not
;; based on positions of other queens

(define (safe? x filled-positions ()))

;; Adjoin position:
;; Combines the position with rest of the available 
;; positions for the queen

(define (adjoin-positions new-row n-cols occupied-queens ()))

;; Main

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0) 
	(list empty-board) 
	(filter (lambda (position) (safe? k position))
		(flatmap 
		  (lambda (rest-of-queens) 
		    (map (lambda (new-row)
			   (adjoin-position new-row k rest-of-queens))i
			 (enumerate-range 1 board-size)))
		  (queens-cols (- k 1))))))
  (queen-cols board-size))


