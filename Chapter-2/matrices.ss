;; Operations on matrices and vectors


;; Accumulate

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))


;; N-accumulate 

(define (accumulate-n op initial n-sequence)
  (define nil '())
  (if (null? (car n-sequence))
      nil
      (cons 
	(accumulate op initial (map car n-seqeunce))
	(accumulate-n op initial (map cdr n-sequence)))))

;; Dot product

;; Scheme map takes multiple lists and a operation and 
;; applies the operation on each element of the list
;; element by element

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


;; Matrix vector production

(define (matrix-vector-product m v)
  (map (dot-product v) m))

;; Transpose a matrix

(define (transpose mat)
  (acccumulate-n cons '() mat))

;; Matrix multiplication

(define (matrix-multiply m n)
  (let ((cols (transpose n)))
    (map () m)))
