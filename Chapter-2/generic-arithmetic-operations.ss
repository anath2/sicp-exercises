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
  (put 'add '(rational rational) (lambda (x y) (add-rat x y)))
  (put 'sub '(rational rational) (lambda (x y) (sub-rat x y)))
  (put 'mul '(rational rational) (lambda (x y) (mul-rat x y)))
  (put 'div '(rational rational) (lambda (x y) (div-rat x y)))
  (put 'make 'rational (lambda (n d) (tag (make-rational n d))))

  'done)


(define (make-rational n d)
  ((get 'make 'rational) n d))
