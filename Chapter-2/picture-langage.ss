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


;; square-limit-2: 
;; Define square limit in terms of square-of-four

(define (square-limit-2 painter n)
  (let ((combine-4 (square-of-four flip-horiz identity rotate-180 flip-vert)))
    (combine-4 (corner-split painter n))))


;; split:
;; Define a general split procedure

(define (split f1 f2)
  (define (recursive painter n)
    (if (= n 0)
      painter
      (let ((smaller (recursive painter (- n 1))))
	(f1 painter (f2 smaller smaller)))))
 (recursive-split))


;; up-split-2:
;; Up split in terms of above defined split

(define up-split-2 (split below besides))


;; right-split-2
;; Right split in terms of split

(define right-split-2 (split besides below))


;; Vector: 
;; A vector a represented in the 2d space (considered here)
;; by x and y co-ordinates. The constructors and selectors 
;; for which are described her

;; Constructor

(define (make-vect x-cor y-cord)
  (cons x-cord y-cord))


;; Selectors 

;; x

(define (xcor-vect vect)
  (car vect))


;; y 

(define (ycor-vect)
  (cadr vect))


;; Procedures

;; add-vect

(define (add-vect v1 v2)
  (make-vect 
    (+ (xcor-vect v1)
       (xcor-vect v2))
    (+ (ycor-vect v1)
       (ycor-vect v2))))


;; sub-vect

(define (sub-vect v1 v2)
  (make-vect
    (- (xcor-vect v1)
       (xcor-vect v2))
    (- (ycor-vect v1)
       (ycor-vect v2))))


;; scale-vect

(define (scale-vect s v)
  (make-vect 
    (* s xcor-vect v)
    (* s xcor-vect v)))


;; frame: 
;; A frame defines how images represented in a plane
;; It consists of 3 selectors : 
;; 1) origin-frame
;; 2) edge-1-frame
;; 3) edge-2-frame


;; make-frame
;; Constructor takes 3 vectors as arguments and 
;; returns a frame

(define (make frame origin edge1 edge2)
  ())


;; selectors

;; origin-frame:

(define (origin-frame frame)
  ()))


;; edge-1-frame

(define (edge-1-frame frame)
  ())


;; edge-2-frame

(define (edge-2-frame frame)
  ())


;; frame-co-ordinates-map

;; Formula : Origin(Frame) + x.Edge-1(Frame) + y.Edge-2(Frame)

(define (frame-coor-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
			    (edge-1-frame frame))
		(scale-vect (ycord-vect v)
			    (edge-2-frame frame))))))



