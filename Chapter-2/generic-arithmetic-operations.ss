;; Define generic arithematic operations on different types

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
