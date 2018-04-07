(let ((pamphlet (string-append (getenv "HOME") "/src/pamphlet/libraries")))
  (add-to-load-path pamphlet))
(use-modules (ice-9 nice-9) (srfi srfi-1) (pamphlet))

(define (lookup key mapping)
  (let* ((((name . value) . remaining) mapping))
    (if (eq? name key)
	value
	(lookup key remaining))))
