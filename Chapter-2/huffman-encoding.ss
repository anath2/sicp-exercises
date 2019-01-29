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
