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

(define (safe? positions)
  (let ((trial (car positions))
	(trial-row (caar positions))
	(trial-column (cadar positions))
	(rest (cdr positions)))
    (accumulate (lambda (pos result)
		  (let ((row (car pos))
			(col (cadr pos)))
		    (and (not (= 
				(- trial-row trial-col)
				(- row col)))
			 (not (= (+ trial-row trial-col) 
				 (+ row col)))
			 (not (= trial-row row))
			 result)))
		true
		rest)))

;; Adjoin position:
;; Combines the position with rest of the available 
;; positions for the queen

(define (adjoin-positions new-row new-col occupied-queens)
  (append occupied-queens (list (list new-row new-col))))

;; Main

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0) 
	(list empty-board) 
	(filter (lambda (positions) (safe? positions))
		(flatmap 
		  (lambda (rest-of-queens) 
		    (map (lambda (new-row)
			   (adjoin-position new-row k rest-of-queens))
			 (enumerate-range 1 board-size)))
		  (queens-cols (- k 1))))))
  (queen-cols board-size))


