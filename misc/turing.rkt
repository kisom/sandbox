#lang racket

(define instruction
  '(("B" "s1") . ("X" "R" "s2")))

;;; The tape utility functions should hide the actual tape
;;; representation.
(define (tape-lookup tape position)
  (unless (hash-has-key? tape position)
    (hash-set! tape position "B"))
  (hash-ref tape position))

(define (tape-write tape position sym)
  (hash-set! tape position sym))

(define (head-update head head_dir)
  (cond
   [(equal? head_dir "R") (+ head 1)]
   [(equal? head_dir "L") (- head 1)]
   [else (error "invalid head direction")]))

(define (turing-update tape head state instruction)
  (tape-write tape head (car instruction))
  (values tape
	  (head-update head (car (cdr instruction)))
	  (car (cdr (cdr instruction)))))

(define (turing-instruction tape instructions)
  (for (instruction instructions)
    ;; does the car match the tape and the head position?
    ;; if it does, break the loop and return the instruction
    ;; keep going
    ))
