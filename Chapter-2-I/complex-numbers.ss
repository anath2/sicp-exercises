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


;; Apply generic operation

(define (apply-generic op . args)
  ())


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

(define (install-rectangular-package)
  ;; Internal procedures
  (define (real-part z) (car z))
  (define (img-part z) (cdr z))
  (define (make-from-real-img x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (img-part z)))))
  (define (angle z)
    (atan (img-part z) (real-part z)))
  (define (make-from-mag-ang r a )
    (cons (* r (cos a)) (* r (sin a))))

(define (tag x) (attach-tag 'rectangular x))

(put 'real-part '(rectangular) real-part)
(put 'img-part '(rectangular) img-part)
(put 'magnitude '(rectangular) magnitude)
(put 'angle '(rectangular) angle)
(put 'make-from-real-img 'rectangular
     (lambda (x y) (tag (make-from-real-img-rectangular x y))))
(put 'make-from-mag-ang 'rectangular
     (lambda (r a) (tag (make-from-mag-ang-rectangular x y))))
'done)


;; Polar representation
'
(define (install-polar-package)
 ;; Internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (img-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-img x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan (y x))))

(define (tag x) (attach-tag 'polar x))

(put 'real-part '(polar) real-part)
(put 'img-part '(polar) img-part)
(put 'magnitude '(polar) magnitude)
(put 'angle '(polar) angle)
(put 'make-from-real-img 'polar
     (lambda (x y) (tag (make-from-real-img-polar x y))))
(put 'make-from-mag-ang 'polar
     (lambda (r a) (tag (make-from-mag-ang-polar r a))))
'done)


;; Apply-generic

;; Apply generic procedure by looking up the definition of the
;; operation from the table

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let proc ((get op type-tags)))
    (if proc
        (apply (map contents args))
        (error "Procedure cannot be found"
               (list op tag-types)))))


;; Selectors based on new representations

(define (real-part z) (apply-generic 'real-part z))


(define (img-part z) (apply-generic 'img-part z))


(define (magnitude z) (apply-generic 'magnitude z))


(define (angle z) (apply-generic 'angle z))


;; Constructors

(define (make-from-real-img x y)
  ((get 'make-from-real-img 'rectangular ) x y))


(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;; Alternative representation using message passing

(define (make-from-real-img x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'img-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown operation on rectangular representation"))))
  dispatch)


(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'img-part) (* r (sin a)))
          (else (error "Unknown operation on polar representation"))))
  dispatch)


(define (apply-generic op arg) (arg op))


;; Excercise 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ;; <more rules>

        (else (error "Unknown expression  type -- DERIV" exp))))


;; Data directed form
;; Note, that the expression is represented by a pair with operator and operands


;; a. The operators are added to a table. When an operation has to be performed,
;; the function is looked up using 'get statement and applied upon the variables with a given
;; expression.
;; number and variable methods here are not added to data directed dispatch because those are
;; predicates and return values instead of function

(define (install-sum)
  (define (make-sum a1 a2) (cons a1 a2))
  (define (addend e) (cadr e))
  (define (augend e) (caddr e))
  (define (deriv-sum e)
    (make-sum (deriv (addend e)) (deriv (augend e))))
  (define (tag x) (attach-tag '+ x))

  (put 'deriv '(+) deriv-sum)
  (put 'make-sum '+ (lambda (a1 a2) (tag (make-sum a1 a2))))
  'done)


(define (make-sum a1 a2)
  ((get 'make-sum '+ ) a1 a2))


(define (install-product)
  (define (make-product p1 p2) (cons p1 p2))
  (define (multiplier e) (cadr e))
  (define (multiplicand e) (caddr e))
  (define (deriv-product p)
    (make-sum
     (make-product (multiplier p)
                   (deriv (multiplicand p)))
     (make-product (multiplicand p)
                   (deriv (mulitplier p)))))

  (define (tag x) (attach-tag '* x))
  (put 'deriv '(*) deriv-product)
  (put 'make-product '* (lambda (p1 p2) (tag (make-product p1 p2))))
  'done)


(define (make-product p1 p2)
  ((get 'make-product '*) p1 p2))


(define (install-exponentiation)
  (define (make-exponentiation base exp) (cons base exp))
  (define (base e) (cadr e))
  (define (exp e) (caddr e))
  (define (deriv-exponentation e)
    (make-product (exp e)
                  (make-exponentiation base (- exp 1))))
  (define (tag x) (attach-tag '^ x))
  (put 'deriv '(^) deriv-exponentiation)
  (put 'make-exponentiation '^ (lambda (base exp) (tag (make-exponentiation base exponent))))
  'done)


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))


(define (operator exp) (car exp))


(define (operands exp) (cdr exp))