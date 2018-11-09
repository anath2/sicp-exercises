;; Eight queen problem: Find all ways to arrange eight queens on the 
;; chess board such that no two queen cancel each other

;; Constants

(define empty-board '())	

(define nil '())	

;; Accumulate:

(define (accumulate op init sequence)
  (if (null? sequence)
      nil
      (op (car sequence)
	  (accumulate op init (cdr sequence)))))

i;; FlatMap

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence))

;; Safe: 
;; Checks if a position is safe to place the queen or not
;; based on positions of other queens

(define (safe? x filled-positions ()))

;; Adjoin position:
;; Combines the position with rest of the available 
;; positions for the queen

(define (adjoin-positions new-row n-cols occupied-queens ()))

;; Queens

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	  (lambda (positions) (safe? k positions))
	  (flatmap
	    (lambda (rest-of-queens)
	      (map (lambda (new-row)
		     (adjoin-position new-row k rest-of-queens))
		   (enumerate-interval 1 board-size)))
	    (queen-cols (- k 1))))))
  (queen-cols board-size))





