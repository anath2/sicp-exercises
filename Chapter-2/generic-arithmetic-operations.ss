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

  'done)
