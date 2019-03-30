;; Define generic arithematic operations on different types

;; Generic scheme numbers

(define (scheme-number-package)

  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (/ x y)))
  (put 'make 'scheme-number (lambda (x) (tag x)))

  'done)


;; Create scheme number

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))


;; Rational numbers

(define (install-rational-package)
  ;; Internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat gcd n d)
    (let (( g (gcd n d)))
      (cons (/ n g) (/d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (denom x))))

  ;; Interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (tag (lambda (x y) (add-rat x y))))
  (put 'sub '(rational rational) (tag (lambda (x y) (sub-rat x y))))
  (put 'mul '(rational rational) (tag (lambda (x y) (mul-rat x y))))
  (put 'div '(rational rational) (tag (lambda (x y) (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rational n d))))

  'done)


(define (make-rational n d)
  ((get 'make 'rational) n d))


;; Complex numbers
;; This package uses some of the procedures defined in complex.ss
;; Additivity permits us to include already defined packages in
;; our definitions


(define (install-complex-package)
  ;; import procedures from complex.ss

  (define (make-from-real-img x y)
    ((get 'make-from-real-img 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))


  ;; Internal procedures

  (define (add-complex z1 z2)
    (make-from-real-img (+ (real-part z1)
                           (real-part z2))
                        (+ (img-part  z1)
                           (img-part  z2))))

  (define (sub-complex x y)
    (make-from-real-img (- (real-part z1)
                           (real-part z2))
                        (- (img-part  z1)
                           (img-part  z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1)
                          (magnitude z2))
                       (* (angle z1)
                          (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1)
                          (magnitude z2))
                       (/ (angle z1)
                          (angle z2))))

  ;; Iterface to the rest of the system

  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))

  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))

  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))

  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))

  (put 'make-from-real-img 'complex
       (lambda (x y) (tag (make-from-real-img x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)


(define (make-from-real-img x y)
  ((get 'make-from-real-img 'complex) x y))


(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;; Generic procedures

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))


(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No methods for these types -- APPLY-GENERIC"
                 (list op type-tags))))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;; -----------------------------------------------------------------


;; Exercises

;; 2.77

(define (install-complex-package)
  ;; import procedures from complex.ss

  (define (make-from-real-img x y)
    ((get 'make-from-real-img 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))


  ;; Internal procedures

  (define (add-complex z1 z2)
    (make-from-real-img (+ (real-part z1)
                           (real-part z2))
                        (+ (imag-part  z1)
                           (imag-part  z2))))

  (define (sub-complex x y)
    (make-from-real-img (- (real-part z1)
                           (real-part z2))
                        (- (imag-part  z1)
                           (imag-part  z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1)
                          (magnitude z2))
                       (* (angle z1)
                          (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1)
                          (magnitude z2))
                       (/ (angle z1)
                          (angle z2))))

  ;; Iterface to the rest of the system

  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))

  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))

  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))

  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))

  (put 'make-from-real-img 'complex
       (lambda (x y) (tag (make-from-real-img x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; New selectors

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)


;; this works by calling the generic selectors defined on the
;; complex datatype

;; Consider the complex number: 3 + 4i
;; (apply-generic 'real-part '(complex) ('complex (rectangular' 3 4))) ->
;; (real-part ('rect 3 4)) ->
;; (apply-generic 'real-part '(rectangular) ('rect 3 4)) ->
;; ((get 'real-part '(rect) ) (3 4))
;; 3
;; Apply generi is called 2 times
;; real part generic selector is called once followed by
;; real part of rectangular package
