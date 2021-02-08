#|
Compute log and log1p with 105-bit accuracy

Both implementations do some argument reduction, find excellent 53-bit initial estimates, and then
perform one Newton step.
|#

(module math.flonum.expansion.log (fp2log fp2log1p)
  (import scheme
          chicken.base
          chicken.type
          chicken.flonum
          math.flonum.functions
          math.flonum.error
          math.flonum.log
          math.flonum.expansion.base
          math.flonum.expansion.exp)

  ;; ===================================================================================================
  ;; log

  #|
  Argument reduction for log:

  log(x) = log(x*2^k) - k*log(2)

  A value of k that reduces any x to [0.5, 1.0] is

  k = -truncate(log(x)/log(2))
  |#

  (define-values (log2-hi log2-lo) (values 0.6931471805599453 2.3190468138462996e-17))

  (: fp2log-reduction (float float -> float float float))
  (define (fp2log-reduction x2 x1)
    (define k (- (fptruncate (fp/ (fplog+ x1 x2) (fplog 2.0)))))
    (cond [(fp> k 1023.0)
           ;; This can happen if x is subnormal; just multiply in pieces
           (let* ([k0 1023.0]
                  [k1 (fp- k k0)]
                  [2^k0 (fpexpt 2.0 k0)]
                  [2^k1 (fpexpt 2.0 k1)])
             [let*-values ([(x2 x1)  (values (* x2 2^k0 2^k1) (* x1 2^k0 2^k1))])
               (values k x2 x1)])]
          [else
           (let ([2^k (fpexpt 2.0 k)])
             (let*-values ([(x2 x1)  (values (fp* x2 2^k) (fp* x1 2^k))])
               (values k x2 x1)))]))

  (: fp2log (float float -> float float))
  (define (fp2log x2 x1)
    (define x (fp+ x1 x2))
    (cond [(fp<= x 0.0)  (cond [(fp= x 0.0)  (values -inf.0 0.0)]
                               [else  (values +nan.0 0.0)])]
          [(fp= x +inf.0)  (values +inf.0 0.0)]
          [(or (fp< x 0.5) (fp> x 2.5))
           ;; Reduce arguments
           (let*-values ([(k x2 x1)  (fp2log-reduction x2 x1)]
                         [(y2 y1)  (fp2log x2 x1)]
                         [(z2 z1)  (fp2* log2-hi log2-lo k)])
             (fp2- y2 y1 z2 z1))]
          [else
           ;; Estimate log(x) and do a Newton iteration using expm1
           (let*-values ([(y)  (fplog+ x2 x1)]
                         [(x2 x1)  (fp2+ x2 x1 -1.0)]
                         [(z2 z1)  (fpexpm1/error y)]
                         [(w2 w1)  (fp2+ z2 z1 1.0)]
                         [(dy2 dy1)  (fp2- x2 x1 z2 z1)]
                         [(dy2 dy1)  (fp2/ dy2 dy1 w2 w1)])
             (fp2+ dy2 dy1 y))]))

  ;; ===================================================================================================
  ;; log1p

  #|
  Argument reduction for log1p:

  log1p(x) = k*log(2) + log1p(x/2^k + (1/2^k - 1))

  A `k'  that reduces any argument `x' to (-1/2,1/2) is

  k = round(log1p(x)/log(2))
  |#

  (: fp2log1p-reduction (float float float float float -> float float float))
  (define (fp2log1p-reduction x2 x1 a2 a1 y)
    (define k (fpround (fp/ y (fplog 2.0))))
    (define 2^k (fpexpt 2.0 k))
    (define-values (j2 j1) (fast-fp-/error (/ 1.0 2^k) 1.0))
    (let*-values ([(x2 x1)  (values (/ x2 2^k) (/ x1 2^k))]
                  [(x2 x1)  (fp2+ x2 x1 j2 j1)])
      (values k x2 x1)))

  (: fp2log1p (float float -> float float))
  (define (fp2log1p x2 x1)
    (define-values (a2 a1) (fp2+ x2 x1 1.0))
    (define y (fplog+ a2 a1))
    (cond
     [(or (fp< y -0.5) (fp> a2 2.0))  (fp2log a2 a1)]
     [(fp= (fp+ x2 x1) 0.0)  (values x2 0.0)]
     [(fp> y 0.5)
      (let*-values ([(k x2 x1)  (fp2log1p-reduction x2 x1 a2 a1 y)]
                    [(y2 y1)  (fp2log1p x2 x1)]
                    [(z2 z1)  (fp2* log2-hi log2-lo k)])
        (fp2+ y2 y1 z2 z1))]
     [else
      (let*-values ([(z2 z1)  (fpexpm1/error y)]
                    [(w2 w1)  (fp2+ z2 z1 1.0)]
                    [(dy2 dy1)  (fp2- x2 x1 z2 z1)]
                    [(dy2 dy1)  (fp2/ dy2 dy1 w2 w1)])
        (fp2+ dy2 dy1 y))])))
