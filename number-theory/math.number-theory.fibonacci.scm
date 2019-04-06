(module math.number-theory.fibonacci (make-fibonacci
                                fibonacci
                                make-modular-fibonacci
                                modular-fibonacci)
  (import scheme
          (only chicken.base error))

  (define (generator a b p q count)
    ;; SICP ex. 1.19
    (cond
     [(zero? count) b]
     [(even? count)
      (generator a b
                 (+ (* p p) (* q q))
                 (+ (* 2 p q) (* q q))
                 (quotient count 2))]
     [else
      (generator (+ (* b q) (* a q) (* a p))
                 (+ (* b p) (* a q))
                 p
                 q
                 (- count 1))]))

  (define ((make-fibonacci a b) n)
    (cond [(< n 0) (error 'fibonacci "bad argument type - not a nonnegative integer" n)]
          [else  (generator b a 0 1 n)]))

  (define fibonacci (make-fibonacci 0 1))

  (define (modular-generator a b p q count mod)
    (cond
     [(zero? count) (modulo b mod)]
     [(even? count)
      (modular-generator
       a b
       (modulo (+ (* p p) (* q q)) mod)
       (modulo (+ (* 2 p q) (* q q)) mod)
       (quotient count 2)
       mod)]
     [else
      (modular-generator
       (modulo (+ (* b q) (* a q) (* a p)) mod)
       (modulo (+ (* b p) (* a q)) mod)
       p
       q
       (- count 1)
       mod)]))

  (define ((make-modular-fibonacci a b) n mod)
    (cond [(< n 0) (error 'modular-fibonacci "bad argument type - not a nonnegative integer" n)]
          [(<= mod 0) (error 'modular-fibonacci "bad argument type - not a positive integer" mod)]
          [else  (modular-generator b a 0 1 n mod)]))

  (define modular-fibonacci (make-modular-fibonacci 0 1)))
