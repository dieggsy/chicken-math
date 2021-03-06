(module math.number-theory.quadratic (complex-quadratic-solutions
                                      quadratic-solutions
                                      quadratic-integer-solutions
                                      quadratic-natural-solutions)
  (import scheme
          chicken.type
          (only chicken.base include conjoin)
          (only srfi-1 filter)
          math.racket-shim)

  (include "math-types.scm")

  (: complex-quadratic-solutions  (number number number -> (list-of number)))
  (define (complex-quadratic-solutions a b c)
    ;; Return list of solutions to a a x^2 + b x + c = 0
    ;; where a,b and c are complex numbers.
    (let* ([d         (- (* b b) (* 4 a c))]
           [sqrt-d    (sqrt d)]
           [-b-sqrt-d (- (- b) sqrt-d)]
           [2a        (* 2 a)])
      (cond
       [(= d 0) (list (/ b (- 2a)))]
       [(= c 0) (list (/ (- (- b) sqrt-d) 2a)
                      (/ (+ (- b) sqrt-d) 2a))]
       ;; use the standard formula, unless -b and sqrt are almost equal
       [#t ; (> (magnitude -b-sqrt-d) 0.001)
        (list (/ -b-sqrt-d        2a)
              (/ (+ (- b) sqrt-d) 2a))]
       [else
        ;; Note: Disabled for now.
        ;;       There are cases where only one root needs to
        ;;       use Muller's formula. But which one is it?
        ;; Muller's formula:
        ;;   x = 2c / ( -b +- sqrt(d) )
        (let* ([sign (if (>= 0 (real-part (* (conjugate b) sqrt-d)))
                         1 -1)]
               [q    (/ (+ b (* sign sqrt-d)) -2)])
          (list (/ q a) (/ c q)))])))

  (: quadratic-solutions (real real real -> (list-of real)))
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

  (: quadratic-integer-solutions (real real real -> (list-of integer)))
  (define (quadratic-integer-solutions a b c)
    ;; return list of integer solutions to a x^2 + b x + c = 0
    (filter (conjoin exact? integer?) (quadratic-solutions a b c)))

  (: quadratic-natural-solutions (real real real -> (list-of integer)))
  (define (quadratic-natural-solutions a b c)
    ;; return list of nonnegative-integer solutions to a x^2 + b x + c = 0
    (filter natural? (quadratic-solutions a b c))))
