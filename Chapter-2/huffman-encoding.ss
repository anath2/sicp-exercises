;; Huffman encoding
;; ----------------


;; Leaf

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr object))

(define (weight-leaf x)
  (caddr object))


;; Tree definition

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))  ;; Complete list of symbols
        (+ (weight left) (weight right))))       ;; Sum of all weights
