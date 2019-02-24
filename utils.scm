(import chicken.base)

(define natural? (conjoin (complement negative?) integer?))

(define (andmap fn ls0)
  (let mapf ((ls ls0))
    (or
     (null? ls)
     (and
      (fn (car ls))
      (mapf (cdr ls))))))
