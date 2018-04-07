#lang racket

;;; a turing machine implementation
(define tape (make-hash))

(define (lookup position)
  (unless (hash-has-key? tape position)
    (hash-set! tape position 0))
  (hash-ref tape position))

(define turing-machine%
  (class object%
    (init)
    (define tape (make-hash))
    (define head 0)
    (define current-state #f)
    (define state-table)
    (super-new)))
