(module math.private.farey (farey-sequence
                            mediant)
  (import scheme
          (only chicken.base error sub1))

  (define (mediant x y)
    (/ (+ (numerator x) (numerator y))
       (+ (denominator x) (denominator y))))

  (define (farey-sequence n)
    (cond [(<= n 0) (error 'farey-sequence "bad argument type - not a positive integer" n)]
          [else
           (let loop ([a 1] [b 1] [c (sub1 n)] [d n] [fs '()])
             (let ([fs  (cons (/ a b) fs)])
               (cond [(positive? a)
                      (let ((k (quotient (+ n b) d)))
                        (loop c d (- (* k c) a) (- (* k d) b) fs))]
                     [else
                      fs])))])))
