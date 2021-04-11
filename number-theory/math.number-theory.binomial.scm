(module math.number-theory.binomial (binomial)
  (import scheme
          chicken.type
          (only chicken.base include error add1)
          (only miscmacros ensure)
          math.racket-shim)

  (include "math-types.scm")

  (: binomial* (integer integer -> integer))
  (define (binomial* n k)
    ;;  compute the binomial coeffecient n choose k
    ;; https://gmplib.org/manual/Binomial-Coefficients-Algorithm.html
    (ensure
     natural?
     (let loop ([n n] [k k])
       (assume ((n exact-rational) (k exact-rational))
         (cond
          [(= k 0) 1]
          [(= k 1) n]
          [(> k n) 0]
          [(= k 2) (/ (* n (- n 1)) 2)]
          [(> k (/ n 2)) (loop n (- n k))]
          [else (* (+ n (- k) 1)
                   (let loop1 ((prod 1)
                               (i 2))
                     (assume ((prod exact-rational)
                              (i integer))
                       (if (> i k)
                           prod
                           (loop1 (* prod (/ (+ n (- k) i) i)) (add1 i))))))])))))

  (: binomial (integer integer -> integer))
  (define (binomial n k)
    (cond [(< n 0) (error 'binomial "bad argument type - not a nonnegative integer" n)]
          [(< k 0) (error 'binomial "bad argument type - not a nonnegative integer" k)]
          [(zero? k) 1]
          [(eqv? n 1) (if (eqv? k 1) 1 (binomial* n k))]
          [else (binomial* n k)])))
