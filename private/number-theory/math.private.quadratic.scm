(module math.private.quadratic (quadratic-solutions
                                quadratic-integer-solutions
                                quadratic-natural-solutions)
  (import scheme
          chicken.base
          srfi-1)

  (define natural? (conjoin (complement negative?) integer?))

  (define (quadratic-solutions a b c)
    ;; return list of solutions to a a x^2 + b x + c = 0
    (let ([d (- (* b b) (* 4 a c))])
      (cond
       [(< d 0) '()]
       [(= d 0) (list (/ b (* -2 a)))]
       [else
        (let ([sqrt-d (sqrt d)])
          (list (/ (- (- b) sqrt-d) (* 2 a))
                (/ (+ (- b) sqrt-d) (* 2 a))))])))

  (define (quadratic-integer-solutions a b c)
    ;; return list of integer solutions to a x^2 + b x + c = 0
    (filter (conjoin exact? integer?) (quadratic-solutions a b c)))

  (define (quadratic-natural-solutions a b c)
    ;; return list of nonnegative-integer solutions to a x^2 + b x + c = 0
    (filter natural? (quadratic-solutions a b c))))
