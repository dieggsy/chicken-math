(module math.flonum.more-functions (fpsqrt1pm1
                                    fpsinh fpcosh fptanh
                                    fpasinh fpacosh fpatanh
                                    make-fpexpt fpexpt+ fpexpt1p
                                    fpfma)
  (import scheme
          chicken.type
          chicken.flonum
          (only chicken.base define-values let-values assert include)
          math.flonum.functions
          math.flonum.constants
          math.flonum.exp
          math.flonum.log
          math.flonum.error
          math.flonum.fpvector
          math.flonum.utils)

  (include "math-types.scm")
  ;; ---------------------------------------------------------------------------------------------------
  ;; sqrt(1+x)-1

  (: fpsqrt1pm1 (float -> float))
  (define (fpsqrt1pm1 x)
    (cond [(fp> (fpabs x) 0.75)
           (fp- (fpsqrt (fp+ 1.0 x)) 1.0)]
          [else
           (fpexpm1 (fp* 0.5 (fplog1p x)))]))

  ;; ---------------------------------------------------------------------------------------------------
  ;; Hyperbolic sine

  (: fpsinh (float -> float))
  (define (fpsinh x)
    (cond [(fp< x 0.0)
           ;; Odd function
           (- (fpsinh (- x)))]
          [(fp< x (fpexpt 2.0 -26.0))
           ;; sinh(x) ~ x
           x]
          [(fp< x 18.5)
           ;; sinh(x) = (exp(2*x) - 1) / (2*exp(x))
           (let ((y (fpexpm1 x)))
             (fp* 0.5 (fp+ y (fp/ y (fp+ y 1.0)))))]
          [(fp< x (fplog +max.0))
           ;; sinh(x) ~ exp(x) / 2
           (fp* 0.5 (fpexp x))]
          [else
           ;; sinh(x) ~ exp(x) / 2 = (exp(x/2) / 2) * exp(x/2)
           (let ((y (fpexp (fp* 0.5 x))))
             (fp* (fp* 0.5 y) y))]))

  ;; ---------------------------------------------------------------------------------------------------
  ;; Hyperbolic cosine

  (: fpcosh (float -> float))
  (define (fpcosh x)
    ;; cosh(x) = cosh(-x)
    (let ([x  (fpabs x)])
      (cond [(fp< x (fpexpt 2.0 -26.0))
             ;; cosh(x) ~ 1
             1.0]
            [(fp< x (fp* 0.5 (fplog 2.0)))
             ;; cosh(x) = 1 + (exp(x) - 1)^2 / (2*exp(x))
             (let ((y (fpexpm1 x)))
               (fp+ 1.0 (fp/ (fp* y y) (fp* 2.0 (fp+ 1.0 y)))))]
            [(fp< x 18.5)
             ;; cosh(x) = (exp(x) + 1/exp(x)) / 2
             (let ((y (fpexp x)))
               (fp+ (fp* 0.5 y) (fp/ 0.5 y)))]
            [(fp< x (fplog +max.0))
             ;; cosh(x) ~ exp(x) / 2
             (fp* 0.5 (fpexp x))]
            [else
             ;; cosh(x) ~ exp(x) / 2 = (exp(x/2) / 2) * exp(x/2)
             (let ((y (fpexp (fp* 0.5 x))))
               (fp* (fp* 0.5 y) y))])))

  ;; ---------------------------------------------------------------------------------------------------
  ;; Hyperbolic tangent

  (: fptanh (float -> float))
  (define (fptanh x)
    (cond [(fp< x 0.0)
           ;; tanh(x) = -tanh(-x)
           (- (fptanh (- x)))]
          [(fp< x 1e-16)
           ;; tanh(x) ~ x + x^2
           (fp* x (fp+ 1.0 x))]
          [(fp< x 0.5)
           ;; tanh(x) = (exp(2*x) - 1) / (exp(2*x) + 1)
           (let ((y (fpexpm1 (fp* -2.0 x))))
             (- (fp/ y (fp+ 2.0 y))))]
          [(fp< x 19.5)
           ;; tanh(x) = (exp(2*x) - 1) / (exp(2*x) + 1)
           (let ((y (fpexp (fp* 2.0 x))))
             (fp/ (fp- y 1.0) (fp+ y 1.0)))]
          [(fp<= x +inf.0)
           ;; tanh(x) ~ 1
           1.0]
          [else  +nan.0]))

  ;; ---------------------------------------------------------------------------------------------------
  ;; Inverse hyperbolic sine

  (: fpasinh (float -> float))
  (define (fpasinh x)
    (cond [(fp< x 0.0)  (- (fpasinh (- x)))]
          [(fp< x 2e-8)  x]
          [(fp< x 0.00018)
           ;; Taylor series order 3
           (fp* x (fp+ 1.0 (fp* (fp* #i-1/6 x) x)))]
          [(fp< x 1.0)
           ;; Standard definition, rearranged to preserve digits
           (fplog1p (fp+ x (fpsqrt1pm1 (fp* x x))))]
          [(fp< x 3e3)
           ;; Standard definition
           (fplog (fp+ x (fpsqrt (fp+ (fp* x x) 1.0))))]
          [(fp< x 1e307)
           ;; Laurent series in 1/x at 0+ order from -1 to 1
           (fp+ (fplog (fp* x 2.0)) (fp/ 1.0 (fp* (fp* 4.0 x) x)))]
          [else
           ;; Laurent series, rearranged to not overfpow
           (fp+ (fplog x) (fplog 2.0))]))

  ;; ---------------------------------------------------------------------------------------------------
  ;; Inverse hyperbolic cosine

  (: fpacosh (float -> float))
  (define (fpacosh x)
    (cond [(fp< x 1.0)  +nan.0]
          [(fp< x 1.5)
           ;; Standard definition, rearranged to preserve digits when x is near 1.0
           (let ((y (fp- x 1.0)))
             (fplog1p (fp+ y (fpsqrt (fp+ (fp* y y) (fp* 2.0 y))))))]
          [(fp< x 1e8)
           ;; Standard definition
           (fplog (fp+ x (fpsqrt (fp- (fp* x x) 1.0))))]
          [(fp< x 1e307)
           ;; Laurent series in 1/x at 0+ order from -1 to 0
           (fplog (fp* x 2.0))]
          [else
           ;; Laurent series, rearranged to avoid overfpow
           (fp+ (fplog x) (fplog 2.0))]))

  ;; ---------------------------------------------------------------------------------------------------
  ;; Inverse hyperbolic tangent

  (: flatanh (float -> float))
  (define (fpatanh x)
    (cond [(fp< x 0.0)  (- (fpatanh (- x)))]
          [(fp< x 1e-8)  x]
          [(fp< x 0.00015)
           ;; Taylor series order 2
           (fp+ x (fp* (fp* (fp* #i1/3 x) x) x))]
          [(fp< x 0.5)
           ;; Standard definition, rearranged to preserve digits when x is near 0.0
           (fp* 0.5 (fp- (fplog1p x) (fplog1p (- x))))]
          [(fp< x 1.0)
           ;; Standard definition
           (fp* 0.5 (fplog (fp/ (fp+ 1.0 x) (fp- 1.0 x))))]
          [(fp= x 1.0)  +inf.0]
          [else  +nan.0]))

  ;; ---------------------------------------------------------------------------------------------------
  ;; Exponential with high-precision bases


  (: make-fpexpt (exact-rational -> (float -> float)))
  (define (make-fpexpt b)
    (assert (positive? b) 'make-fpexpt "not a positive ratnum:" b)
    (define b-hi (exact->inexact b))
    (define b-lo (exact->inexact (- (/ (inexact->exact b-hi) b) 1)))
    (cond [(fp= b-lo 0.0)  (lambda (x) (fpexpt b-hi x))]
          [else
           (lambda (x)
             (fp/ (fpexpt b-hi x)
                  (fpexp (fp* x (fplog1p b-lo)))))]))

  (: fpexpt+ (float float float -> float))
  (define (fpexpt+ a b y)
    (define-values (x-hi x-lo) (fast-fp+/error a b))
    (fp/ (fpexpt x-hi y)
         (fpexp (fp* y (fplog1p (- (/ x-lo x-hi)))))))

  (: fpexpt1p (float float -> float))
  (define (fpexpt1p x y)
    (cond [(and (> x -0.5) (< x +inf.0))
           (let-values ([(a-hi a-lo) (fast-fp+/error 1.0 x)])
             (fp/ (fpexpt a-hi y)
                  (fpexp (fp* y (fplog1p (- (/ a-lo a-hi)))))))]
          [else  (fpexpt (+ 1.0 x) y)]))

  ;; ---------------------------------------------------------------------------------------------------
  ;; Fused multiply-add

  (: slow-fpfma (float float float -> float))
  (define (slow-fpfma a b c)
    (define n (near-pow2 (max (fpsqrt (abs a)) (fpsqrt (abs b)))))
    (define 1/n (/ 1.0 n))
    (* n n (fast-fpfma (* a 1/n) (* b 1/n) (* c 1/n 1/n))))


  (: fast-flfma (float float float -> float))
  (define (fast-fpfma a b c)
    (let-values ([(d-hi d-lo)  (fast-fpfma/error a b c)])
      (+ d-hi d-lo)))

  (: flfma (float float float -> float))
  (define (fpfma a b c)
    (let ([d  (fast-fpfma a b c)])
      (if (fprational? d) d (slow-fpfma a b c)))))
