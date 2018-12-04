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


(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))


;; selectors

(define (select-elem items index)
  (define nil '())
  (cond
    ((null? items) nil)
    ((= index 0) (car items))
    (else (select-elem (cdr items) (- index 1)))))


;; origin-frame:

(define (origin-frame frame)
  (select-elem frame 0))


;; edge-1-frame

(define (edge-1-frame frame)
  (select-elem frame 1))


;; edge-2-frame

(define (edge-2-frame frame)
  (select-elem frame 2))


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


;; Segment painter:
;; Draws figures based on a list of line segments


(define (segments->painter segment-list)
  (lambda (frame)
    (for-each  ;; Draw segment
      (lambda (segment)4
	(draw-line
	  ((frame-coor-map frame) (start-segment segment))
	  ((frame-coor-map frame) (end-segment segment))))
      segment-list)))


;; Directed Line
;; A line segment is represented by a vector
;; from origin to the beginning of the line segment
;; and a vector from origin to the end of the segment

(define (make-segment start-vect end-vect)
  (cons start-vect end-vect))


;; Selectors

(define (start-segment segment)
  (car segment))


(define (end-segment segment)
  (cadr segment))


;; Draw using segment painter:


(let  ((tl (make-vect 0 1))
       (tr (make-vect 1 1))
       (bl (make-vect 0 0))
       (br (make-vect 1 0))))


;; Frame rectangle

(define (draw-frame)
  (segment->painter
   (list (make-segment bl tl)
         (make-segment bl br)
         (make-segment br tr)
         (make-segment tl tr))))


;; 'X'

(define (draw-x)
  (segment->painter
   (list (make-segment bl tr)
         (make-segment br tl))))


;; Diamond joining mid points of frame

(let ((tl-mid (make-vect 0 0.5))
      (t-mid  (make-vect 0.5 1))
      (tr-mid (make-vect 1 0.5))
      (b-mid  (make-vect 0.5 0))))


(define (draw-diamond)
  (segment->painter
   (list (make-segment b-mid tl-mid)
         (make-segment tl-mid t-mid)
         (make-segment t-mid tr-mid)
         (make-segment tr-mid b-mid))))


;; Wave

(define (draw-wave)
  ())


;; Painter transformation

(define (transform-painter painter origin corner-1 corner-2)
  (lambda (frame)
    (let ((m frame-coor-map))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner-1) new-origin)
                     (sub-vect (m corner-2) new-origin)))
        ))))


;; flip-vert: Flip the painter vertically

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0 0)))


;; flip-horiz: Flip the painter horizontally

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0)
                     (make-vect 0 0)
                     (make-vect 1.0 1.0)))


;; Shrink to top right corner

(define (shrink-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


;; Rotate frame 90 degrees counter clockwise

(define (rotate-90 painter)
  (transform-painter painter
                     (make-vect 1.0 0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


;; Squash frame inwards

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


;; Besides:
;; Function that displays painters next to each others

(define (beside painter-1 painter-2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
           (transform-painter painter-1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter-2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
