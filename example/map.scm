
(define (map f xs)
  (if (>< xs)
    []
    (: (f (<- xs)) (map f (-> xs)))))

(define add-one (lambda (x) (+ 1 x)))

; test
(map add-one [1 2 3 4 5 3 4 3 2 4 true])
