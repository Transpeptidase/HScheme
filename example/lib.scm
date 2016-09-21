
(define (add x y) (+ x y))
(define (sub x y) (- x y))
(define (mul x y) (* x y))
(define (div x y) (/ x y))
(define (remainder x y) (% x y))
(define (negate x) (- x))

(define (and x y) (&& x y))
(define (or x y) (|| x y))
(define (not x) (! x))

(define (eq? x y) (= x y))
(define (ne? x y) (!= x y))
(define (greater? x y) (> x y))
(define (lower? x y) (< x y))
(define (ge? x y) (>= x y))
(define (le? x y) (<= x y))

(define (cons x xs) (: x xs))
(define (head xs) (<- xs))
(define (tail xs) (-> xs))
(define (car xs) (<- xs))
(define (cdr xs) (-> xs))
(define null [])
(define (null? xs) (>< xs))

(define (even? x) (= (% x y) 0))
(define (odd? x) (= (% x y) 1))

(define (map f xs) 
  (if (>< xs)
    []
    (: (f (<- xs)) (map f (-> xs)))))

(define (filter f xs)
  (if (>< xs)
    []
    (if (f (<- xs))
      (: (<- xs) (filter f (-> xs))))))

(define (foldr f init xs)
  (if (>< xs)
    []
    (f (foldr f init (-> xs)) (<- xs))))

(define (foldl f init xs)
  (if (>< xs)
    []
    (foldl f (f (<- xs) init) (-> xs))))
