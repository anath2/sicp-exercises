;; Complex numbers as defined and represented
;; using a rectangular representation and polar representation


;; Constructor

((define make-from-real-img real img) (cons real img))

((define make-from-mag-ang mag ang) (cons mag ang))


;; Selectors

(define (real-part z) (car z))

(define (img-part z) (cadr z))

(define (magnitude z) (car z))

(define (angle z) (cadr z))


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
