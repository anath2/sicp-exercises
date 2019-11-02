;; Defining accumulator 

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


;; Basic list operations

;; Map

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
	      '() sequence))

;; Append 

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;; Length

(define (length sequence)
  (accumulate (lambda (a b) (+ 1 b)) 0 sequence))
