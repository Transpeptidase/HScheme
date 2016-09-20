; #lang racket

(define (divide? a b)
  (= (% b a) 0))

(define square (lambda (x) (* x x)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))
  
; test
(prime? 1)
(prime? 2)
(prime? 17)
(prime? 1111)
(prime? 111111)
