(module math.number-theory.polygonal (triangle-number
                                triangle-number?
                                square-number?
                                pentagonal-number pentagonal-number?
                                hexagonal-number hexagonal-number?
                                heptagonal-number heptagonal-number?
                                octagonal-number octagonal-number?)
  (import scheme
          (only chicken.base include)
          (only math.number-theory.quadratic quadratic-natural-solutions)
          (only miscmacros ensure)
          (only math.number-theory.base perfect-square))

  (include "utils.scm")

  (define (triangle-number n)
    (quotient (* n (+ n 1)) 2))

  (define (triangle-number? n)
    (not (null? (quadratic-natural-solutions 1/2 1/2 (- n)))))

  (define (square-number? n)
    (and (perfect-square n) #t))

  (define (pentagonal-number n)
    (ensure natural? (quotient (* n (- (* 3 n) 1)) 2)))

  (define (pentagonal-number? n)
    (not (null? (quadratic-natural-solutions 3/2 -1/2 (- n)))))

  (define (hexagonal-number n)
    (ensure natural? (* n (- (* 2 n) 1))))

  (define (hexagonal-number? n)
    (not (null? (quadratic-natural-solutions 2 -1 (- n)))))

  (define (heptagonal-number n)
    (ensure natural? (quotient (* n (- (* 5 n) 3)) 2)))

  (define (heptagonal-number? n)
    (not (null? (quadratic-natural-solutions 5/2 -3/2 (- n)))))

  (define (octagonal-number n)
    (ensure natural? (* n (- (* 3 n) 2))))

  (define (octagonal-number? n)
    (not (null? (quadratic-natural-solutions 3 -2 (- n))))))
