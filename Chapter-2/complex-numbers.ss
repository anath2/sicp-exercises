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


;; Complex numbers as defined and represented

;; using a rectangular representation and polar representation

;; Check underlying repr

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))


(define (polar? z)
  (eq? (type-tag z) 'polar))


;; Rectangular representation

(define (real-part-rectangular z) (car z))


(define (img-part-rectangular z) (cdr z))


(define (mag-rectangular z)
  (sqrt (+ (square (real-part z)) (square (img-part z))))


(define (ang-rectangular z)
  (atan (img-part z) (real-part z)))


;; Constructor

((define make-from-real-img-rectangular real img)
 (attach-tag 'rectangular (cons real img)))


((define make-from-mag-ang-rectangular r a)
 (attach-tag 'rectangular
             (cons (* r (cos a)) (* r (sin b)))))


(define (make-from-real-img x y)
  (make-from-real-img-rectangular x y))


(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))


;; Polar representation

(define (real-part-polar z)
  (* (mag z) (cos (ang z))))


(define (img-part-polar z)
  (* (mag z) (sin (ang z))))


(define (mag-polar z) (car z))


(define (ang-polar z) (cdr z))


(define (make-from-real-img-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x)) (+ (square y)))
                    (atan y x))))


(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
              (cons r a)))


;; Generic selectors

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type - REAL PART" z))))


(define (img-part z)
  (cond ((rectangular? z)
         (img-part-rectangular (contents z)))
        ((polar? z)
         (img-part-polar (contents z)))
        (else (error "Unknown type - IMG PART" z))))


(define (magnitude z)
  (cond ((rectangular? z)
         (mag-rectangular (contents z)))
        ((polar? z)
         (mag-polar (contents z)))
        (else (error "Unknown type - MAG" z))))


(define (angle z)
  (cond ((rectangular? z)
         (ang-rectangular (contents z)))
        ((polar? z)
         (ang-polar (contents z)))
        (else (error "Unknown type - ANG" z))))


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


;; Interface to the rest of the system in terms of
;; data driven

;; Rectangular representation

(define (tag x) (attach-tag 'rectangular x))

(put 'real-part '(rectangular) real-part)
(put 'img-part '(rectangular) img-part)
(put 'magnitude '(rectangular) magnitude)
(put 'angle '(rectangular) angle)
(put 'make-from-real-img 'rectangular
     (lambda (x y) (tag (make-from-real-img-rectangular x y))))
(put 'make-from-mag-ang 'rectangular
     (lambda (r a) (tag (make-from-mag-ang-rectangular x y))))


;; Polar representation

(define (tag x) (attach-tag 'polar x))

(put 'real-part '(polar) real-part)
(put 'img-part '(polar) img-part)
(put 'magnitude '(polar) magnitude)
(put 'angle '(polar) angle)
(put 'make-from-real-img 'polar
     (lambda (x y) (tag (make-from-real-img-polar x y))))
(put 'make-from-mag-ang 'polar
     (lambda (r a) (tag (make-from-mag-ang-polar r a))))
