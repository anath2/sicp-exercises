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

;;up-split: 
;; Split the painter function recursively upwardsi
(define (up-split painter n)
  (if (= n 0)
      painter
      (let (smaller (up-split painter (- n 1)))
	
	)
      )
  )
