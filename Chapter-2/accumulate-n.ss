;; Accumulate multiple lists one element from each list at a time
;; assuming all the lists are equal in size

;; Accumulate 

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;; N-Accumulate 

(define (accumulate-n op initial n-sequence)
  (define nil '())	
  (if (null? (car n-sequence))
      nil	
      (cons 
	(accumulate op initial (map car n-sequence))
	(accumulate-n op initial (map cdr n-sequence)))))


