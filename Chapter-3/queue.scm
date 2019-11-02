;; A queue, also known as a FIFO buffer is a mutable datastructure
;; The items inserted from one end (rear) of the queue and deleted
;; from the other end

;; A queue consists of the following methods
;; Constructor - (make-queue)

;; Constructor


(define (make-queue) (cons '() '()))
