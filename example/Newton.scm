
(define (square x) (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good_enough guess pre)
  (< (abs (- guess pre)) 0.001))

(define (sqrt_iter guess x pre)
  (if (good_enough guess pre)
      guess
      (sqrt_iter (improve guess x)
                 x guess)))

(define (sqrt x)
  (sqrt_iter 1.0 x 0.0))

; test 
(sqrt 100)
(sqrt 2)
(sqrt 3)
