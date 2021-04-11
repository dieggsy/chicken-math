#|
Arithmetic based on:

Jonathan Richard Shewchuk
Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates
Discrete & Computational Geometry 18(3):305–363, October 1997

Other parts shamelessly stolen from crlibm (which is LGPL)
|#

(module math.flonum.expansion.base (fp2?
                                    fp2zero?
                                    fp2rational?
                                    fp2positive?
                                    fp2negative?
                                    fp2infinite?
                                    fp2nan?

                                    fp2
                                    fp2->real

                                    fp2ulp
                                    fp2ulp-error
                                    fp2step
                                    fp2next
                                    fp2prev

                                    +max.hi
                                    +max.lo
                                    -max.hi
                                    -max.lo

                                    +max-subnormal.hi
                                    -max-subnormal.hi

                                    fp2abs
                                    fp2+
                                    fp2-
                                    fp2=
                                    fp2>
                                    fp2<
                                    fp2>=
                                    fp2<=

                                    fp2*split-fp
                                    fp2*
                                    fp2sqr
                                    fp2/

                                    fp2sqrt
                                    fpsqrt/error)

  (import scheme
          chicken.base
          chicken.type
          chicken.flonum
          chicken.bitwise
          (only miscmacros define-syntax-rule)
          math.flonum.functions
          math.flonum.bits
          math.flonum.error
          math.flonum.constants
          math.flonum.utils
          math.racket-shim)


  (include "math-types.scm")

  (: floverlapping? (float float -> boolean))
  (define (fpoverlapping? x2 x1)
    (define-values (s2 e2) (flonum->sig+exp (fpabs x2)))
    (define-values (s1 e1) (flonum->sig+exp (fpabs x1)))
    (define-values (n1 n2)
      (if (> e2 e1)
          (values s1 (arithmetic-shift s2 (- e2 e1)))
          (values (arithmetic-shift s1 (- e1 e2)) s2)))
    (not (= (bitwise-ior n1 n2)
            (bitwise-xor n1 n2))))

  (: fl2? (float float -> boolean))
  (define (fp2? x2 x1)
    (cond [(rational? x2)
           (cond [(rational? x1)
                  (cond [(< (fpabs x2) (fpabs x1))  #f]
                        [else (not (fpoverlapping? x2 x1))])]
                 [else  #f])]
          [else
           (fp= x1 0.0)]))

  (define-syntax-rule (define-simple-fp2-predicate fp2pred? fppred?)
    (begin
      (: fp2pred? (float float -> boolean))
      (define (fp2pred? x2 x1)
        (fppred? (fp+ x2 x1)))))

  (define-simple-fp2-predicate fp2zero? (lambda (x) (fp= x 0.0)))
  (define-simple-fp2-predicate fp2positive? (lambda (x) (fp> x 0.0)))
  (define-simple-fp2-predicate fp2negative? (lambda (x) (fp< x 0.0)))
  (define-simple-fp2-predicate fp2rational? rational?)
  (define-simple-fp2-predicate fp2nan? nan?)
  (define-simple-fp2-predicate fp2infinite? infinite?)

  ;; ===================================================================================================
  ;; Conversion

  (: fp2  (real #!optional float -> float float))
  (define fp2
    (case-lambda
      [(x)
       (cond [(flonum? x)  (values x 0.0)]
             ;; [(single-flonum? x)  (values (fl x) 0.0)]
             [else
              (let ([x2 (fp x)])
                (if (infinite? x2)
                    (values x2 0.0)
                    (let* ([x  (- x (inexact->exact x2))]
                           [x1  (fp x)]
                           [x  (- x (inexact->exact x1))])
                      (let-values ([(x2 x1)  (fp+/error x2 x1)])
                        (values x2 (fp+ x1 (fp x)))))))])]
      [(x2 x1)
       (if (and (fp= x2 0.0) (fp= x1 0.0))
           (values x2 0.0)
           (fp+/error x2 x1))]))

  (: fp2eqv? (float float float #!optional float -> boolean))
  (define (fp2eqv? x2 x1 y2 #!optional [y1 0.0])
    (and (eqv? x2 y2) (fp= x1 y1)))

  (: fp2->real (float float -> real))
  (define (fp2->real x2 x1)
    (if (rational? x2)
        (+ (inexact->exact x2) (inexact->exact x1))
        x2))

  (: fp4->fp2 (float float float float -> float float))
  (define (fp4->fp2 e4 e3 e2 e1)
    (values e4 (fp+ e3 (fp+ e2 e1))))

  ;; ===================================================================================================
  ;; Error

  (: fp2ulp (float float -> float))
  (define (fp2ulp x2 x1)
    (cond [(fp= x2 0.0)  0.0]
          [else  (fpmax +min.0 (fp* (fpulp x2) epsilon.0))]))

  (: fp2ulp-error (float float real -> float))
  (define (fp2ulp-error x2 x1 r)
    (define x (fp2->real x2 x1))
    (define-values (r2 r1) (fp2 r))
    (cond [(eqv? x r)  0.0]
          [(and (fp= x2 0.0) (fp= r2 0.0))  0.0]
          [(and (fp= x2 +inf.0) (fp= r2 +inf.0))  0.0]
          [(and (fp= x2 -inf.0) (fp= r2 -inf.0))  0.0]
          [(zero? r)  +inf.0]
          [(and (rational? x) (rational? r2))
           (fpabs (fp (/ (- (inexact->exact x) (inexact->exact r))
                         (inexact->exact (fpmax +min.0 (fp2ulp r2 r1))))))]
          [else  +inf.0]))

  (define-values (+max.hi +max.lo)
    (values +max.0 (fpprev (* 0.5 (fpulp +max.0)))))

  (define-values (-max.hi -max.lo)
    (values (- +max.hi) (- +max.lo)))

  (: fp2step (float float integer -> float float))
  (define (fp2step x2 x1 n)
    (let-values ([(x2 x1)  (fast-fp+/error x2 x1)])
      (cond [(fpnan? x2)  (values +nan.0 0.0)]
            [(fp= x2 +inf.0)  (fp+/error +max.hi (fpstep +max.lo (+ n 1)))]
            [(fp= x2 -inf.0)  (fp+/error -max.hi (fpstep -max.lo (- n 1)))]
            [else  (fp+/error x2 (fpstep x1 n))])))

  (: fp2next (float float -> float float))
  (define (fp2next x2 x1) (fp2step x2 x1 1))

  (: fp2prev (float float -> float float))
  (define (fp2prev x2 x1) (fp2step x2 x1 -1))

  (define +min-normal.hi (fp/ (fpnext +max-subnormal.0) epsilon.0))

  (define-values (+max-subnormal.hi +max-subnormal.lo)
    (fp2prev +min-normal.hi 0.0))

  (define-values (-max-subnormal.hi -max-subnormal.lo)
    (values (- +max-subnormal.hi) (- +max-subnormal.lo)))

  ;; ===================================================================================================
  ;; Absolute value

  (: fp2abs (float #!optional float -> float float))
  (define fp2abs
    (case-lambda
      [(x)  (values (fpabs x) 0.0)]
      [(x2 x1)
       (cond [(fpnan? x2)  (values +nan.0 0.0)]
             [(fp= x2 0.0)  (values 0.0 0.0)]
             [(fp> x2 0.0)  (values x2 x1)]
             [else  (values (- x2) (- x1))])]))

  ;; ===================================================================================================
  ;; Addition and subtraction

  (: fp2+ (float float float #!optional float -> float float))
  (define (fp2+ x2 x1 y2 #!optional [y1 0.0])
    (define r (fp+ x2 y2))
    (cond [(not (rational? r))  (values r 0.0)]
          [(and (fp= x2 0.0) (fp= y2 0.0))  (values r 0.0)]
          [else
           (let* ([s (if (fp> (fpabs x2) (fpabs y2))
                         (fp+ (fp+ (fp+ (fp- x2 r) y2) y1) x1)
                         (fp+ (fp+ (fp+ (fp- y2 r) x2) x1) y1))]
                  [z2 (fp+ r s)])
             (values z2 (fp+ (fp- r z2) s)))]))

  (: fp2- (float float float #!optional float -> float float))
  (define (fp2- x2 x1 y2 #!optional [y1 0.0])
    (fp2+ x2 x1 (- y2) (- y1)))

  ;; ===================================================================================================
  ;; Comparison

  (define-syntax-rule (define-fp2-comparison name fpcomp)
    (begin
      (: name (float float float float -> boolean))
      (define (name x2 x1 y2 y1)
        (let-values ([(z2 z1)  (fp2- x2 x1 y2 y1)])
          (fpcomp (fp+ z2 z1) 0.0)))))

  (define-fp2-comparison fp2= fp=)
  (define-fp2-comparison fp2> fp>)
  (define-fp2-comparison fp2< fp<)
  (define-fp2-comparison fp2>= fp>=)
  (define-fp2-comparison fp2<= fp<=)

  ;; ===================================================================================================
  ;; Multiplication and division

  (: raw-split-fp2*split-fp (float float float float float float
                                   -> float float float float))
  (define (raw-split-fp2*split-fp e2-hi e2-lo e1-hi e1-lo b-hi b-lo)
    (let*-values ([(b)   (fp+ b-lo b-hi)]
                  [(Q1)  (fp* (fp+ e1-lo e1-hi) b)]
                  [(h1)  (- (- Q1
                               (fp* e1-hi b-hi)
                               (fp* e1-lo b-hi)
                               (fp* e1-hi b-lo)
                               (fp* e1-lo b-lo)))]
                  [(T)  (fp* (fp+ e2-lo e2-hi) b)]
                  [(t)  (- (- T
                              (fp* e2-hi b-hi)
                              (fp* e2-lo b-hi)
                              (fp* e2-hi b-lo)
                              (fp* e2-lo b-lo)))]
                  [(Q2 h2)  (fast-fp+/error Q1 t)]
                  [(h4 h3)  (fast-mono-fp+/error T Q2)])
      (values h4 h3 h2 h1)))

  (: split-fp2*split-fp (float float float float float float -> float float))
  (define (split-fp2*split-fp e2-hi e2-lo e1-hi e1-lo b-hi b-lo)
    (let-values ([(h4 h3 h2 h1)  (raw-split-fp2*split-fp e2-hi e2-lo e1-hi e1-lo b-hi b-lo)])
      (fp4->fp2 h4 h3 h2 h1)))

  (: fp2*split-fp (float float float float -> float float))
  (define (fp2*split-fp e2 e1 b-hi b-lo)
    (let*-values ([(e2-hi e2-lo)  (fpsplit e2)]
                  [(e1-hi e1-lo)  (fpsplit e1)]
                  [(h4 h3 h2 h1)  (raw-split-fp2*split-fp e2-hi e2-lo e1-hi e1-lo b-hi b-lo)])
      (fp4->fp2 h4 h3 h2 h1)))

  (: fp2* (float float float #!optional float -> float float))
  (define (fp2* x2 x1 y2 #!optional [y1 0.0])
    (define z (fp* x2 y2))
    (cond [(fp= z 0.0)  (values z 0.0)]
          [(fpsubnormal? z)  (values z 0.0)]
          [(and (rational? x2) (rational? y2) (fp>= z -inf.0) (fp<= z +inf.0))
           (let* ([dx (near-pow2 x2)]
                  [dy (near-pow2 y2)]
                  [d (fp* dx dy)]
                  [d? (and (fp> d 0.0) (fp< d +inf.0))]

                  [x2  (fp/ x2 dx)]
                  [x1  (fp/ x1 dx)]
                  [y2  (fp/ y2 dy)]
                  [y1  (fp/ y1 dy)]
                  [up  (fp* x2 (fp+ 1.0 (fpexpt 2.0 27.0)))]
                  [vp  (fp* y2 (fp+ 1.0 (fpexpt 2.0 27.0)))]
                  [u1  (fp+ (fp- x2 up) up)]
                  [v1  (fp+ (fp- y2 vp) vp)]
                  [u2  (fp- x2 u1)]
                  [v2  (fp- y2 v1)]
                  [m2  (fp* x2 y2)]
                  [m1  (fp+ (fp+ (fp+ (fp+ (fp+ (fp- (fp* u1 v1) m2)
                                                (fp* u1 v2))
                                           (fp* u2 v1))
                                      (fp* u2 v2))
                                 (fp* x2 y1))
                            (fp* x1 y2))]
                  [z2  (fp+ m2 m1)]
                  [z1  (fp+ (fp- m2 z2) m1)]
                  [z2  (if d? (fp* z2 d) (fp* (fp* z2 dx) dy))])
             (values z2 (if (rational? z2) (if d? (fp* z1 d) (fp* (fp* z1 dx) dy)) 0.0)))]
          [else
           (values z 0.0)]))

  (: fp2sqr (float #!optional float -> float float))
  ;; Derived from fl2*
  (define fp2sqr
    (case-lambda
      [(x)  (fpsqr/error x)]
      [(x2 x1)
       (define z (fp* x2 x2))
       (cond [(fp= z 0.0)  (values z 0.0)]
             [(fpsubnormal? z)  (values z 0.0)]
             [(and (rational? x2) (fp>= z -inf.0) (fp<= z +inf.0))
              (let* ([dx (near-pow2 x2)]
                     [d (fp* dx dx)]
                     [d? (and (fp> d 0.0) (fp< d +inf.0))]
                     [x2  (fp/ x2 dx)]
                     [x1  (fp/ x1 dx)]
                     [up  (fp* x2 (fp+ 1.0 (fpexpt 2.0 27.0)))]
                     [u1  (fp+ (fp- x2 up) up)]
                     [u2  (fp- x2 u1)]
                     [m2  (fp* x2 x2)]
                     [m1  (fp+ (fp+ (fp+ (fp- (fp* u1 u1) m2)
                                         (fp* 2.0 (fp* u1 u2)))
                                    (fp* u2 u2))
                               (fp* 2.0 (fp* x2 x1)))]
                     [z2  (fp+ m2 m1)]
                     [z1  (fp+ (fp- m2 z2) m1)]
                     [z2  (if d? (fp* z2 d) (fp* (fp* z2 dx) dx))])
                (values z2 (if (rational? z2) (if d? (fp* z1 d) (fp* (fp* z1 dx) dx)) 0.0)))]
             [else
              (values z 0.0)])]))

  (: fp2/ (float float float #!optional float -> float float))
  (define (fp2/ x2 x1 y2 #!optional [y1 0.0])
    (define z (fp/ x2 y2))
    (cond [(and (rational? z) (not (fp= z 0.0)) (rational? y2))
           (let ([d (near-pow2/div x2 y2)])
             (let*-values ([(x2 x1)  (values (fp/ x2 d) (fp/ x1 d))]
                           [(y2 y1)  (values (fp/ y2 d) (fp/ y1 d))]
                           [(c2)  (fp/ x2 y2)]
                           [(u2 u1)  (fp*/error c2 y2)]
                           [(c1)  (fp/ (fp- (fp+ (fp- (fp- x2 u2) u1) x1) (fp* c2 y1)) y2)]
                           [(z2)  (fp+ c2 c1)])
               (values z2 (if (rational? z2) (fp+ (fp- c2 z2) c1) 0.0))))]
          [else
           (values z 0.0)]))

  ;; ===================================================================================================
  ;; Square roots

  (: fl2sqrt (float #!optional float -> float float))
  ;; One-flonum estimate followed by one Newton's method iteration
  (define (fp2sqrt x2 #!optional [x1 0.0])
    (cond [(and (rational? x2) (not (fp= x2 0.0)))
           (let*-values ([(d^2 d) (cond [(fp<= x2 +max-subnormal.hi)  (values (fpexpt 2.0 -104.0)
                                                                              (fpexpt 2.0 -52.0))]
                                        [(fp> x2 1e300)  (values (fpexpt 2.0 104.0)
                                                                 (fpexpt 2.0 52.0))]
                                        [else  (values 1.0 1.0)])]
                         [(x2 x1)  (values (fp/ x2 d^2) (fp/ x1 d^2))]
                         [(y)  (fpsqrt (fp+ x2 x1))]
                         [(z2 z1)  (fast-fpsqr/error y)]
                         [(dy2 dy1)  (fp2- x2 x1 z2 z1)]
                         [(dy2 dy1)  (fp2/ dy2 dy1 y)]
                         [(y2 y1)  (fp2+ (fp* 0.5 dy2) (fp* 0.5 dy1) y)]
                         [(y2)  (fp* y2 d)])
             (values y2 (if (rational? y2) (fp* y1 d) 0.0)))]
          [else
           (values (fpsqrt x2) 0.0)]))

  (: fpsqrt/error (float -> float float))
  (define (fpsqrt/error x) (fp2sqrt x 0.0)))
