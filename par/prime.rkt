(define (prime-numbers amount from)
  (if (= amount 0)
    '()
    (if (prime? from)
      (cons from (prime-numbers (- amount 1) (+ from 1)))
      (prime-numbers amount (+ from 1)))))

(define (divisors n from)
  (if (> from n)
    '()
    (if (= (remainder n from) 0)
      (cons from (divisors n (+ from 1)))
      (divisors n (+ from 1)))))

(define (prime? number)
  (equal? (divisors number 1) (list 1 number)))

(define (square x) (* x x))

(define (squares list-of-numbers)
  (if (eq? list-of-numbers '())
    '()
    (cons (square (first list-of-numbers)) (squares (rest of numbers)))))

(define (map f list)
  (if (eq? list '())
    '()
    (cons (f (first list)) (plural f (rest list)))))

(define (sum numbers)
  (if (eq? numbers '())
    0
    (+ (first numbers) (sum (rest numbers)))))

;; (sum (map square (prime-numbers 7 1)))
