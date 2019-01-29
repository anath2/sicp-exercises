;; Huffman encoding
;; ----------------


;; Leaf

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf object)
  (cadr object))

(define (weight-leaf object)
  (caddr object))


;; Tree constructor

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))  ;; Complete list of symbols
        (+ (weight left) (weight right))))       ;; Sum of all weights


;; Tree selectors

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))  ;; Since leaf of tree is always a list
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
