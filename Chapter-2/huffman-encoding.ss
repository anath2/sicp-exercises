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


;; Creates an initial ordering for combining sets of elements

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let (pair (car pairs))
        (adjoin-set (make-leaf (car pair)   ;; Symbol
                               (cadr pair)) ;; Weight
                    (make-left-set (cdr pairs))))))


;; Adjoin an element to a set
;; The elements are added in a way that they are ascending order
;; by weight.

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


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

(define (encode-message message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode-message (cdr message) tree))))


(define (encode-symbol symbol tree)

  (define (correct-branch symbol branch)
    (if (leaf? branch)
        (eq? (symbol-leaf) symbol)
        (member? (symbols branch) symbol)))

  (let ((lb (left-branch tree))
        (rb (right-branch tree)))
    (cond ((correct-branch symbol lb)
           (if (leaf? lb) '(0) (cons 0 (encode-symbol lb))))
          ((correct-branch symbol rb)
           (if (leaf? rb) '(1) (cons 1 (encode-symbol rb))))
          (else (error "symbol not present")))))


(define (member? symbols elem)
  (cond ((null? symbols) false)
        ((eq? (car symbols) elem) true)
        (else (member? (cdr symbols) elem))))


;; Ex 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge leaf-set)  ;; leaf-set in ascending order
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((first (car leaf-set))
            (second (cadr leaf-set))
            (rest (cddr leaf-set)))
        (successive-merge (adjoin-set (make-code-tree first second)
                                      rest)))))  ;; Arrange once again in ascending order


;; Ex 2.70

;; Symbol tree
;;
;; A        2        NA      16
;; BOOM     1        SHA     3
;; GET      2        YIP     9
;; JOB      2        WAH     1
;;
;;
;; Message
;;
;; Get a job
;; Sha na na na na na na na na
;; Get a job
;; Sha na na na na na na na na
;; Wah yip yip yip yip yip yip yip yip yip
;; Sha boom

(define (songtree (generate-huffman-tree '((A, 2),
                                           (NA, 2),
                                           (BOOM 1),
                                           (SHA 3),
                                           (GET 2),
                                           (YIP 9),
                                           (JOB 2),
                                           (WAH 1)))))


;; Leaf set:
;;
;; (list (leaf' WAH 1)
;;       (leaf' BOOM 1)
;;       (leaf' NA 2)
;;       (leaf' A 2)
;;       (leaf' JOB 2)
;;       (leaf' GET 2)
;;       (leaf SHA 3)
;;       (leaf' YIP 9))
;;
;; Tree:
;;
;; ((leaf na 16)
;; ((leaf yip 9) (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4) ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7) (a wah boom sha job get) 11) (yip a wah boom sha job get) 20)
;; (na yip a wah boom sha job get) 36)


;; Ex. 2.71
