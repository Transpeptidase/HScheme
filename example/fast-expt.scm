
; a * (b ^ n)
; = a * (b ^ 2) ^ (n / 2)       [even? n]
; = (a * b) * b ^ (n - 1)       [odd? n]

(define (fast_expt b n)
  (let ((aux (lambda (a b n)
               (cond ((= n 0) a)
                     ((even? n) (aux a (* b b) (/ n 2)))
                     (else (aux (* a b) b (- n 1)))))))
  (aux 1 b n)))

  
; test
(fast_expt 7 100000) 
