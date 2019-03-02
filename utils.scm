(import (only chicken.base conjoin complement exact-integer?))

(define natural? (conjoin (complement negative?) exact-integer?))

(define (andmap fn ls0)
  (let mapf ((ls ls0))
    (or
     (null? ls)
     (and
      (fn (car ls))
      (mapf (cdr ls))))))

(define (ormap func ls0 . rest)
  (and
   (pair? ls0)
   (let ((rest (cons ls0 rest)))
     (or
      (apply func (map car rest))
      (apply ormap func (map cdr rest))))))
