(module math.number-theory.quadratic (quadratic-solutions
                                quadratic-integer-solutions
                                quadratic-natural-solutions)
  (import scheme
          chicken.type
          (only chicken.base include conjoin)
          (only srfi-1 filter))

  (include "utils.scm")

  (: quadratic-solutions (number number number -> (list-of number)))
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

  (: quadratic-integer-solutions (number number number -> (list-of integer)))
  (define (quadratic-integer-solutions a b c)
    ;; return list of integer solutions to a x^2 + b x + c = 0
    (filter (conjoin exact? integer?) (quadratic-solutions a b c)))

  (: quadratic-natural-solutions (number number number -> (list-of integer)))
  (define (quadratic-natural-solutions a b c)
    ;; return list of nonnegative-integer solutions to a x^2 + b x + c = 0
    (filter natural? (quadratic-solutions a b c))))
