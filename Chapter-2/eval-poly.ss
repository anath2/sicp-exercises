;; Evaluate a polynomial using horner's rule

;; Defining accumulator

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) 
	  (accumulate op initial (cdr sequence)))))

;; Horner polynomial evaluation

(define (horner-eval x cofficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)  (+  this-coeff  (* higher-terms x))
	      0
	      cofficient-sequence))


