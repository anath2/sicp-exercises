;; Complex numbers as defined and represented
;; using a rectangular representation and polar representation


;; Selectors

(define (real-part z) (car z))


(define (img-part z) (cdr z))


(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (img-part z))))


(define (angle z)
  (atan (img-part z) (real-part z)))


;; Constructor

((define make-from-real-img real img) (cons real img))


((define make-from-mag-ang r a)
 (cons (* r (cos a)) (* r (sin b))))


;; Polar form repr

(define (real-part z)
  (* (mag z) (cos (ang z))))


(define (img-part z)
  (* (mag z) (sin (ang z))))


(define (mag z) (car z))


(define (ang z) (cdr z))


(define (make-from-real-img x y)
 (cons (sqrt (+ (square x)) (+ (square y)))
       (atan y x)))


(define (make-from-mag-ang r a) (cons r a))


;; Multiplication and addition of complex numbers

(define (add-complex z1 z2)
  (make-from-real-img
    (+ (real-part z1) (real-part z2))
    (+ (img-part z1) (img-part z2))))


(define (sub-complex z1 z2)
  (make-from-real-img
    (- (real-part z1) (real-part z2))
    (- (img-part z1) (img-part z2))))


(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))


(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))


;; Tagging different representations

(define (attach-tag tag-type contents)
  (cons tag-type contents))


(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "BAD TAGGED DATUM - error getting tag type")))


(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "BAD TAGGED DATUM - error getting tag type contents")))
