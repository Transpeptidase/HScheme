module BasicLib (initEnv) where

import Interpret

import qualified Data.Map   as M

lib = "\
\(define (add x y) (+ x y))\
\(define (sub x y) (- x y))\
\(define (mul x y) (* x y))\
\(define (div x y) (/ x y))\
\(define (remainder x y) (% x y))\
\(define (negate x) (- x))\
\\
\(define (and x y) (&& x y))\
\(define (or x y) (|| x y))\
\(define (not x) (! x))\
\\
\(define (eq? x y) (= x y))\
\(define (ne? x y) (!= x y))\
\(define (greater? x y) (> x y))\
\(define (lower? x y) (< x y))\
\(define (ge? x y) (>= x y))\
\(define (le? x y) (<= x y))\
\\
\(define (cons x xs) (: x xs))\
\(define (head xs) (<- xs))\
\(define (tail xs) (-> xs))\
\(define (car xs) (<- xs))\
\(define (cdr xs) (-> xs))\
\(define null [])\
\(define (null? xs) (>< xs))\
\\
\(define (even? x) (= (% x 2) 0))\
\(define (odd? x) (= (% x 2) 1))\
\(define (abs x) (if (< x 0) (- x) x)) \
\\
\ (define (expt b n) \
\ (let ((aux (lambda (a b n) \
\              (cond ((= n 0) a)\
\                    ((even? n) (aux a (* b b) (/ n 2)))\
\                    (else (aux (* a b) b (- n 1)))))))\
\ (aux 1 b n)))\
\\
\(define (map f xs)\
\  (if (>< xs)\
\    []\
\    (: (f (<- xs)) (map f (-> xs)))))\
\\
\(define (filter f xs)\
\  (if (>< xs)\
\    []\
\    (if (f (<- xs))\
\      (: (<- xs) (filter f (-> xs)))\
\      (filter f (-> xs))))) \
\\
\(define (foldr f init xs)\
\  (if (>< xs)\
\    init\
\    (f (foldr f init (-> xs)) (<- xs))))\
\\
\(define (foldl f init xs)\
\  (if (>< xs)\
\    init\
\    (foldl f (f (<- xs) init) (-> xs))))\
\\
\    "

initEnv = snd $ interpret' lib M.empty
