(module math.number-theory.modular-arithmetic ((with-modulus current-modulus-param)
                                               current-modulus
                                               modular-inverse
                                               modular-expt
                                               mod+
                                               mod-
                                               mod*
                                               mod/
                                               modsqr
                                               modexpt
                                               mod
                                               mod=
                                               mod<
                                               mod<=
                                               mod>
                                               mod>=)
  (import scheme
          chicken.type
          (only chicken.base include-relative case-lambda include)
          (only srfi-1 fold)
          (only math.number-theory.divisibility bezout coprime?)
          (only miscmacros ensure define-syntax-rule))

  (include "math-types.scm")
  (include-relative "modular-arithmetic-base.scm")

  (define-syntax-rule (define-comparison-op name op)
    (begin
      (: name (integer #!rest integer -> boolean))
      (define name
        (case-lambda
          [(a)  #t]
          [(a b)
           (define n (current-modulus))
           (op (modulo a n) (modulo b n))]
          [(a b . cs)
           (define n (current-modulus))
           (let ([a  (modulo a n)] [b  (modulo b n)])
             (and (op a b)
                  (let loop ([b b] [cs cs])
                    (or (null? cs)
                        (let ([c  (modulo (car cs) n)])
                          (and (op b c) (loop c (cdr cs))))))))]))))

  (: mod+ (#!rest integer -> integer))
  (define mod+
    (case-lambda
      [()  0]
      [(a)  (modulo a (current-modulus))]
      [(a b)  (modulo (+ a b) (current-modulus))]
      [(a b . cs)
       (define n (current-modulus))
       (fold (lambda (x y) (modulo (+ x y) n))
             (modulo (+ a b) n)
             cs)]))

  (: mod- (integer #!rest integer -> integer))
  (define mod-
    (case-lambda
      [(a)  (modulo (- a) (current-modulus))]
      [(a b)  (modulo (- a b) (current-modulus))]
      [(a b . cs)
       (define n (current-modulus))
       (fold (lambda (x y) (modulo (- x y) n))
             (modulo (- a b) n)
             cs)]))

  (: mod* (#!rest integer -> integer))
  (define mod*
    (case-lambda
      [() (modulo 1 (current-modulus))]
      [(a)  (modulo a (current-modulus))]
      [(a b)  (modulo (* a b) (current-modulus))]
      [(a b . cs)
       (define n (current-modulus))
       (fold (lambda (x y) (modulo (* x y) n))
             (modulo (* a b) n)
             cs)]))

  (: mod/ (integer #!rest integer -> integer))
  (define mod/
    (case-lambda
      [(a)  (modular-inverse a (current-modulus))]
      [(a b)
       (define n (current-modulus))
       (modulo (* a (modular-inverse b n)) n)]
      [(a b . cs)
       ;; It's ridiculous that `cs' has to be split here
       (mod/ a (apply mod* b (car cs) (cdr cs)))]))

  (: modsqr (integer -> integer))
  (define (modsqr a) (modulo (* a a) (current-modulus)))

  (: modexpt (integer integer -> integer))
  (define (modexpt a b)
    (define n (current-modulus))
    (cond [(< b 0)  (modular-expt (modular-inverse a n) (- b) n)]
          [else  (modular-expt a b n)]))

  (: mod (exact-rational -> integer))
  (define (mod a)
    (cond [(integer? a)  (modulo a (current-modulus))]
          [else  (mod/ (numerator a) (denominator a))]))

  (define-comparison-op mod= =)
  (define-comparison-op mod< <)
  (define-comparison-op mod<= <=)
  (define-comparison-op mod> >)
  (define-comparison-op mod>= >=))
