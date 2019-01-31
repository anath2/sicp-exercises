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


;; Decode procedure

;; Decodes a sequence of bits given a huffman tree
;; The decode procedure is similar to search and accumulate
;; procedure on a binary search tree
;; A sequence of bits represent a symbol, the decoder traverses
;; across the tree until the symbol is found

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) next-branch))

              (decode-1 (cdr bits) next-branch)))))

  (decode-1 bits tree))


;; choose-branch

;; Chose branch procedure selects the left branch if the bit
;; is 0 otherwise chooses the right branch

(define (choose-branch bit tree)
  (cond ((= 0 bit) (left-branch tree))
        ((= 1 bit) (right-branch tree))
        (else (error "bad bit CHOOSE BRANCH" bit))))
