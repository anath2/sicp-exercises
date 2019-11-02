#lang Racket/base

;; Rules
;; (deriv constant r) => 0
;; (deriv r r) => 1
;; (deriv (u + v) r) => (+ (deriv u r) (deriv v r))
;; (deriv (u * v) r) => (+ (* u (deriv v r)) (* v (deriv u r))))


;; Utilities

(define (accumulate proc init seq)
  (if (null? seq)
      init
      (proc
       (car seq)
       (accumulate proc init (cdr seq)))))

(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

;; Constructors

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and
          (number? a1)
          (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-sum-infix a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and
          (number? a1)
          (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))


(define (make-product p1 p2)
  (cond ((=number? p1 1) p2)
        ((=number? p2 1) p1)
        ((or (=number? p1 0)
             (=number? p2 0))
         0)
        ((and (number? p1)
              (number? p2))
         (* p1 p2))
        (else (list '* p1 p2))))


(define (make-product-infix p1 p2)
  (cond ((=number? p1 1) p2)
        ((=number? p2 1) p1)
        ((or (=number? p1 0)
             (=number? p2 0))
         0)
        ((and (number? p1)
              (number? p2))
         (* p1 p2))
        (else (list p1 '* p2))))


(define (make-exponent base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))


(define (make-exponent-infix base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list base '** exponent))))


;; Selectors

(define (addend sum)
  (cadr sum))

(define (addend-infix sum)
  (car sum))

(define (augend sum)
  (accumulate make-sum 0 (cddr sum))) ;; Augend is the sum of all elements expect first

(define (augend-infix sum)
  (caddr sum))
  
(define (multiplier product)
  (cadr product))

(define (multiplier-infix product)
  (car product))

(define (multiplicand product)
  (accumulate make-product 1 (cddr product))) ;; Multiplicand is the product of all elements except first

(define (multiplicand-infix product)
  (caddr product))

(define (base exp)
  (cadr exp))

(define (base-infix exp)
  (car exp))

(define (exponent exp)
  (caddr exp))

(define (exponent-infix exp)
  (caddr exp))

;; Predicates

(define (variable? x)
  ;; We use symbols to represent variables
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? expression)
  (and
   (pair? expression)
   (eq? (car expression) '+)))

(define (sum-infix? expression)
  (and
   (pair? expression)
   (eq? (cadr expression) '+)))

(define (product? expression)
  (and
   (pair? expression)
   (eq? (car expression) '*)))

(define (product-infix? expression)
  (and
   (pair? expression)
   (eq? (cadr expression) '*)))

(define (exponent? expression)
  (and
   (pair? expression)
   (= (length expression) 3)
   (eq? (car expression) '**)))

(define (exponent-infix? expression)
  (and
   (pair? expression)
   (= (length expression) 3)
   (eq? (cadr expression) '**)))

;; Derivative

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum
          (deriv (addend exp) var)
          (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponent? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponent (base exp)
                          (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unkown expression type -- DERIV" exp))))

(define (deriv-infix exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum-infix? exp)
         (make-sum-infix
          (deriv-infix (addend-infix exp) var)
          (deriv-infix (augend-infix exp) var)))
        ((product-infix? exp)
         (make-sum-infix
          (make-product-infix (multiplier-infix exp)
                              (deriv-infix (multiplicand-infix exp) var))
          (make-product-infix (multiplicand-infix exp)
                              (deriv-infix (multiplier-infix exp) var))))
        ((exponent-infix? exp)
         (make-product-infix
          (make-product-infix
           (exponent-infix exp)
           (make-exponent-infix (base-infix exp)
                          (- (exponent-infix exp) 1)))
          (deriv-infix (base-infix exp) var)))
        (else
         (error "unkown expression type -- DERIV" exp))))