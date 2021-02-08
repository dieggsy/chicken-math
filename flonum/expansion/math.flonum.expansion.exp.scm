#|
Note from original (racket) author:

Compute exp and expm1 with 105-bit accuracy

In case anybody cares, the argument reduction scheme used here is about a 3x improvement in speed
over current state-of-the-art for computing exponentials using a Taylor series. The main discovery
that enables it is that it's perfectly okay-and-accuracy-preserving to subtract powers of a base
other than 2, such as 1.25. See "expansion-exp-reduction.rkt" for details.

I may turn this into a paper someday...

Note to self: exp(x+y)-1 = (exp(x)-1) * (exp(y)-1) + (exp(x)-1) + (exp(y)-1)
|#

(module math.flonum.expansion.exp (fpexp/error
                                   fp2exp
                                   fpexpm1/error
                                   fp2expm1)
  (import scheme
          chicken.base
          chicken.type
          chicken.flonum
          chicken.fixnum
          math.base
          ;; math.bigfloat
          math.flonum.functions
          math.flonum.error
          math.flonum.exp
          math.flonum.constants
          math.flonum.expansion.base
          math.flonum.expansion.exp-reduction
          math.racket-shim)
  ;; ===================================================================================================
  ;; Helper functions

  (define-values (c3-hi c3-lo) (fp2 1/6))
  (define-values (c4-hi c4-lo) (fp2 1/24))
  (define-values (c5-hi c5-lo) (fp2 1/120))
  (define-values (c6-hi c6-lo) (fp2 1/720))
  (define-values (c7-hi c7-lo) (fp2 1/5040))
  (define-values (c8-hi c8-lo) (fp2 1/40320))

  (: fpexpm1-small/error (float -> float float))
  ;; Computes exp(x)-1 when x is small, using 8 terms from its Taylor series using Horner's method
  ;; Relative error is <= 1e-32 when (abs x) < 0.0005
  ;; Relative error for (+ 1 (flexpm1-small/error x)) is <= 1e-32 when (abs x) < 0.00118
  (define (fpexpm1-small/error x)
    (let*-values ([(x-hi x-lo)  (fpsplit x)]
                  [(y2 y1)  (fp2*split-fp c8-hi c8-lo x-hi x-lo)]
                  [(y2 y1)  (fp2+ y2 y1 c7-hi c7-lo)]
                  [(y2 y1)  (fp2*split-fp y2 y1 x-hi x-lo)]
                  [(y2 y1)  (fp2+ y2 y1 c6-hi c6-lo)]
                  [(y2 y1)  (fp2*split-fp y2 y1 x-hi x-lo)]
                  [(y2 y1)  (fp2+ y2 y1 c5-hi c5-lo)]
                  [(y2 y1)  (fp2*split-fp y2 y1 x-hi x-lo)]
                  [(y2 y1)  (fp2+ y2 y1 c4-hi c4-lo)]
                  [(y2 y1)  (fp2*split-fp y2 y1 x-hi x-lo)]
                  [(y2 y1)  (fp2+ y2 y1 c3-hi c3-lo)]
                  [(y2 y1)  (fp2*split-fp y2 y1 x-hi x-lo)]
                  [(y2 y1)  (fp2+ y2 y1 0.5)]
                  [(y2 y1)  (fp2*split-fp y2 y1 x-hi x-lo)]
                  [(y2 y1)  (fp2+ y2 y1 1.0)])
      (fp2*split-fp y2 y1 x-hi x-lo)))

  (: fpexpm1-tiny/error (float -> float float))
  ;; Computes exp(x)-1 when x is friggin' tiny, like 1e-18 or something, using 2 terms from its Taylor
  ;; series at 0
  ;; I haven't bothered quantifying the error because this is only used for the low-order flonum in
  ;; fl2exp and fl2expm1, and those are accurate
  (define (fpexpm1-tiny/error x)
    (let-values ([(y2 y1)  (fast-fp+/error (* 0.5 x) 1.0)])
      (fp2* y2 y1 x)))

  ;; ===================================================================================================
  ;; exp

  ;; See "expansion-exp-reduction.rkt" for details on the argument reduction here

  (: fpexp/positive/error (float -> float float))
  (define (fpexp/positive/error x)
    (let loop ([x x] [n 0])
      (cond
       [(or (fp< (fpabs x) exp-min) (fx> n 15))  ; even n > 5 should never happen
        (let-values ([(y2 y1)  (fpexpm1-small/error x)])
          (fp2+ y2 y1 1.0))]
       [(fp> x (fplog +max.0))  (values +inf.0 0.0)]
       [(rational? x)
        (let*-values ([(x d2 d1)  (fpexpm1-reduction x)]
                      [(d2 d1)  (fp2+ d2 d1 1.0)]
                      [(y2 y1)  (loop x (fx+ n 1))])
          (fp2* d2 d1 y2 y1))]
       [else
        (values +nan.0 0.0)])))

  (: fpexp/error (float -> float float))
  (define (fpexp/error x)
    (cond [(fp> x 0.0)  (fpexp/positive/error x)]
          [(fp= x 0.0)  (values 1.0 0.0)]
          [(fp> x (- (fplog +max.0)))
           (let*-values ([(y2 y1)  (fpexp/positive/error (- x))])
             (fp2/ 1.0 0.0 y2 y1))]
          [(fp>= x -inf.0)  (values (fpexp x) 0.0)]
          [else  (values +nan.0 0.0)]))

  (: fp2exp (float float -> float float))
  (define (fp2exp x2 x1)
    (cond [(fp> x2 -746.0)
           (let*-values ([(a2 a1)  (fpexp/error x2)]
                         [(b2 b1)  (fpexpm1-tiny/error x1)]
                         [(b2 b1)  (fp2+ b2 b1 1.0)])
             (fp2* a2 a1 b2 b1))]
          [(fp>= x2 -inf.0)
           (values 0.0 0.0)]
          [else
           (values +nan.0 0.0)]))

  ;; ===================================================================================================
  ;; expm1

  #|
  Argument reduction for expm1

  Let `y' be chosen using exp's argument reduction, D = exp(y)-1, and R = exp(x-y)-1. Then

  exp(x)-1 = D * R + D + R

  using the identity noted at the beginning of this file. Calculating this straightforwardly suffers
  from severe cancellation (generally invalidating all but two bits in the low-order flonum), but
  calculating this doesn't:

  exp(x)-1 = D * (R + 1) + R

  The computation of R+1 definitely loses precision, but it doesn't seem to matter.
  |#

  (: fpexpm1/error (float -> float float))
  (define (fpexpm1/error x)
    (cond
     [(fp> x -1.0)
      (let loop ([x x] [n 0])
        (cond [(or (fp< (fpabs x) expm1-min) (fx> n 15))  ; even n > 5 should never happen
               (fpexpm1-small/error x)]
              [(fp< x (- (fplog +max.0)))  (values -1.0 0.0)]
              [(fp> x (fplog +max.0))  (values +inf.0 0.0)]
              [(rational? x)
               (let*-values ([(x d2 d1)  (fpexpm1-reduction x)]
                             [(r2 r1)  (loop x (fx+ n 1))]
                             [(w2 w1)  (fp2+ r2 r1 1.0)]
                             [(y2 y1)  (fp2* d2 d1 w2 w1)])
                 (fp2+ y2 y1 r2 r1))]
              [else
               (values +nan.0 0.0)]))]
     [(fp> x (fplog (* 0.25 epsilon.0)))
      ;; exp(x) is near zero here, so this is more accurate
      (let-values ([(y2 y1)  (fpexp/error x)])
        (fp2+ y2 y1 -1.0))]
     [else
      ;; The high-order fponum is -1 here, so return -1 + exp(x) directly
      (values -1.0 (fpexp x))]))

  (define-values (expm1-max-hi expm1-max-lo)
    (values 709.782712893384 2.3691528222554853e-14))

  (: fp2expm1 (float float -> float float))
  (define (fp2expm1 x2 x1)
    (define x (fp+ x2 x1))
    (cond
     [(fp< x -1.0)
      (let-values ([(y2 y1)  (fp2exp x2 x1)])
        (fp2+ y2 y1 -1.0))]
     [(fp2>= x2 x1 expm1-max-hi expm1-max-lo)
      (values +inf.0 0.0)]
     [(fp= x 0.0)
      (values x2 0.0)]
     [else
      (let*-values ([(a2 a1)  (fpexpm1/error x2)]
                    [(b2 b1)  (fpexpm1-tiny/error x1)]
                    [(w2 w1)  (fp2+ a2 a1 1.0)]
                    [(y2 y1)  (fp2* b2 b1 w2 w1)])
        (fp2+ y2 y1 a2 a1))])))
