;; Painter module: 
;; A function for drawing images on screen. This function can then be executed recursively
;; or as a part of other functions to generate more complex images


;; besides: 
;; Create a copy of the painter besides the original painter


;; below:
;; Create a copy of the painter below the original painter


;; right-split: 
;; Call the painter function recursively to right

(define (right-split painter n)
  (if (= n 0)
      painter
      (let (smaller (right-split painter (- n 1)))
	(besides (below smaller smaller)))))

;; up-split: 
;; Split the painter function recursively upwardsi

(define (up-split painter n)
  (if (= n 0)
      painter
      (let (smaller (up-split painter (- n 1)))
	(below (besides smaller smaller))))))


;; corner-split:
;; Split painter function recursively to the up and right corner

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split (- n 1))))
      (let ((top-left (beside up up))
	    (bottom-right (below right right))
	    (corner (corner-split (- n 1))))
	(beside (below painter top-left)
		(below bottom-right corner))))))


;; square-limit:
;; If we place corner functions and four corners of 
;; a square, we obtain this pattern

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (besides (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


;; square-of-four:
;; Apply 4 transformations to elements and place them
;; on the corners of a square

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (besides (tl painter) (tr painter)))
	  (bottom (besides (bl painter) (br painter))))
      (below bottom top))))


;; Define flipped-4:
;; Define flipped four in terms of square-of-four function
;; described above

(define (flipped-pairs painter)
 (let ((combine-4 (square-of-four identity flip-vert identity flip-vert)))
   (combine-4 painter)))


;;square-limit-2: 
;; Define square limit in terms of square-of-four


(define (square-limit-2 painter n)
  (let ((combine-4 (square-of-four flip-horiz identity rotate-180 flip-vert)))
    (combine-4 (corner-split painter n))))


