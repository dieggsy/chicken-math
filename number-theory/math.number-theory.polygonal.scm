(module math.number-theory.polygonal (triangle-number
                                triangle-number?
                                square-number?
                                pentagonal-number pentagonal-number?
                                hexagonal-number hexagonal-number?
                                heptagonal-number heptagonal-number?
                                octagonal-number octagonal-number?)
  (import scheme
          chicken.type
          (only chicken.base include)
          (only math.number-theory.quadratic quadratic-natural-solutions)
          (only miscmacros ensure)
          (only math.number-theory.base perfect-square)
          math.racket-shim)

  (: triangle-number (integer -> integer))
  (define (triangle-number n)
    (quotient (* n (+ n 1)) 2))

  (: triangle-number? (integer -> boolean))
  (define (triangle-number? n)
    (not (null? (quadratic-natural-solutions 1/2 1/2 (- n)))))

  (: square-number? (integer -> boolean))
  (define (square-number? n)
    (and (perfect-square n) #t))

  (: pentagonal-number (integer -> integer))
  (define (pentagonal-number n)
    (ensure natural? (quotient (* n (- (* 3 n) 1)) 2)))

  (: pentagonal-number? (integer -> boolean))
  (define (pentagonal-number? n)
    (not (null? (quadratic-natural-solutions 3/2 -1/2 (- n)))))

  (: hexagonal-number (integer -> integer))
  (define (hexagonal-number n)
    (ensure natural? (* n (- (* 2 n) 1))))

  (: hexagonal-number? (integer -> boolean))
  (define (hexagonal-number? n)
    (not (null? (quadratic-natural-solutions 2 -1 (- n)))))

  (: heptagonal-number (integer -> integer))
  (define (heptagonal-number n)
    (ensure natural? (quotient (* n (- (* 5 n) 3)) 2)))

  (: heptagonal-number? (integer -> boolean))
  (define (heptagonal-number? n)
    (not (null? (quadratic-natural-solutions 5/2 -3/2 (- n)))))

  (: octagonal-number (integer -> integer))
  (define (octagonal-number n)
    (ensure natural? (* n (- (* 3 n) 2))))

  (: octagonal-number? (integer -> boolean))
  (define (octagonal-number? n)
    (not (null? (quadratic-natural-solutions 3 -2 (- n))))))
