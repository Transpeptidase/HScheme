; #lang racket

; a * (b ^ n)
; = a * (b ^ 2) ^ (n / 2)       [even? n]
; = (a * b) * b ^ (n - 1)       [odd? n]

(define (even x) (= (% x 2) 0))

(define (aux a b n)
    (cond ((= n 0) a)
          ((even n) (aux a (* b b) (/ n 2)))
          (else (aux (* a b) b (- n 1)))))

(define (fast_expt b n)
  (aux 1 b n))
  
; test
(fast_expt 2 10)
(fast_expt 3 21)
(fast_expt 7 100000) 
