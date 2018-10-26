;; Evaluate a polynomial using horner's rule

;; Defining accumulator

(define (accumulate op initial sequence)
  (if (null? sequence)
      '()
      (op initial (accumulate op initial (cdr sequence)))))


(define (horner-eval x cofficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (..))
	      0
	      cofficient-sequence))
