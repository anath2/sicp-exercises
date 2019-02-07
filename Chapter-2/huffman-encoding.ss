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


;; Adjoin an element to a set
;; The elements are added in a way that they are ascending order
;; by weight.

(define (adjoin-set set x)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


;; Creates an initial ordering for combining sets of elements

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let (pair (car pairs))
        (adjoin-set (make-leaf (car pair)   ;; Symbol
                               (cadr pair)) ;; Weight
                    (make-left-set (cdr pairs))))))


;; Ex 2.67

(define (sample-tree
         (make-code-tree (make-leaf 'A 4)
                         (make-code-tree
                          (make-leaf 'B 2)
                          (make-code-tree
                           (make-leaf 'D 1)
                           (make-leaf 'C 1))))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; decode the message using the above defined decode procedure

;; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
;; '(A D     A B   B   C     A)


;; Ex 2.68


;; TODO

(define (encode-message message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode-message (cdr message) tree))))


(define (encode-symbol symbol tree)
  (define (encode symbol tree binary-string)
    (cond ((not (member? (symbols tree) symbol)) (error "Symbol not in tree"))
          ((leaf? tree) binary-string))
          ((member? (left tree) symbol) (encode symbol (left tree) (cons 0 binary-string)))
          (else (encode symbol (right tree) (cons 1 binary-string))))
  (encode symbol tree '()))


(define (member? symbols elem)
  (cond ((null? symbols) false)
        ((eq? (car symbols) elem) true)
        (else (member? (cdr symbols) elem))))


;; Ex 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
