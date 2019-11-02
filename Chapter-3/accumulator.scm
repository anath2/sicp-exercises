;; Accumulator maintains state, adding removing values

(define (make-accumulator init-value)
  (define (accumulate num)
    (begin
      (set! init-value (+ init-value num))
      init-value))
  accumulate)
