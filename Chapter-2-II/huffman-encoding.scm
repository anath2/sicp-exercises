;; Huffman encoding
;; Variable length encoding. The encoding is based on the
;; frequency of the symbol in the corpus. The symbols are
;; represented as leaves of a binary tree and the traversal
;; through the tree starting at root with left child represented
;; by 0 and right child represented by 1 to arrive at the leaf is
;; the encoding for the symbol

;; Leaf
;; Constructor
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

;; Predicate
(define (leaf? object)
  (eq? (car object) 'leaf))

;; Selectors
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))


;; Tree
;; Constructor
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

;; Selectors
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  
  (decode-1 bits tree))


(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
	((= bit 1) (right-branch tree))
	(else (display "INVALID BIT -- CHOOSE BRANCH" bit))))
      

;; adjoin set procedure is used for constructing encoding tree
;; The set of symbols and tree are arranged in increasing order
;; of weight
(define (adjoin-set x set)
  (cond  ((null? set) (list x))
	 ((< (weight x) (weight (car set)))
	  (cons x set))
	 (else (cons (car set)
		     (adjoin-set x (cdr set))))))


;; Make leaf set pair. From a list of leaves and correspondig pairs,
;; Create an initial ordering from which the huffman tree is to be
;; generated. The leaves are arranged in increasing order
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)  ;; Symbol
			       (cdr pair)) ;; Weight
		    (make-leaf-set (cdr pairs))))))


;; Example
(define example-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

;; Example message
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; Decoding the above message with the example tree
;; adabbca


;; Encode takes a tree and a message and produces a coded message
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (find-symbol symbol tree)
    (cond ((leaf? tree) '())
		((element-of-set? symbol (symbols (left-branch tree)))
	   (cons 0 (find-symbol symbol (left-branch tree))))
	  (else (cons 1 (find-symbol symbol (right-branch tree))))))
  
  (if (element-of-set? symbol (symbols tree))
      (find-symbol symbol tree)
      (display "ENCODE-SYMBOL-- symbol not in tree")))
  
(define (element-of-set? x set)
  (cond ((null? set) '#f)
	((equal? x (car set)) '#t)
	(else (element-of-set? x (cdr set)))))


;; Create huffman tree
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


;; Merges leaves into trees and 
(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((first  (car leaf-set))
	    (second (cadr leaf-set))
	    (rest   (cddr leaf-set)))
	(successive-merge (adjoin-set (make-code-tree first second) rest)))))
