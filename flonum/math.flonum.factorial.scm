(module math.flonum.factorial (fpfactorial
                               fpbinomial
                               fppermutations
                               fpmultinomial
                               fplog-factorial
                               fplog-permutations
                               fplog-binomial
                               fplog-multinomial)
  (import scheme
          chicken.base
          chicken.flonum
          chicken.type
          math.number-theory.factorial
          math.functions.stirling-error
          math.flonum.functions
          math.flonum.log
          math.flonum.more-functions
          math.flonum.error
          math.racket-shim)

  ;; ===================================================================================================
  ;; Factorial

  (: fpfactorial (float -> float))
  ;; Error = 0 ulps
  (define (fpfactorial n)
    (cond [(not (integer? n))  +nan.0]
          [(fp< n 0.0)  +nan.0]
          [(fp< n 171.0)  (fp (factorial (fp->fx n)))]
          [else  +inf.0]))

  (: fplog-factorial (float -> float))
  ;; Error <= 1 ulp
  (define (fplog-factorial n)
    (cond [(not (integer? n))  +nan.0]
          [(fp< n 0.0)  +nan.0]
          [(fp< n 171.0)  (fplog (fp (factorial (fp->fx n))))]
          [else  (+ (fpstirling n)
                    (* 0.5 (fplog (* 2.0 pi n)))
                    (* n (- (fplog n) 1.0)))]))

  ;; ===================================================================================================
  ;; Binomial

  (: fpbinomial (float float -> float))
  ;; Error <= 4 ulps
  (define (fpbinomial n k)
    (cond [(not (integer? n))  +nan.0]
          [(not (integer? k))  +nan.0]
          [(fp< n 0.0)   +nan.0]
          [(fp<= k 0.0)  (if (fp= k 0.0) 1.0 +nan.0)]
          [(fp>= k n)    (if (fp= k n) 1.0 0.0)]
          [(fp> k (fp/ n 2.0))  (fpbinomial n (fp- n k))]
          [(fp< n 171.0)  (fpround (/ (fpfactorial n) (fpfactorial k) (fpfactorial (- n k))))]
          [else
           (let*-values ([(n-k) (- n k)]
                         [(a-hi a-lo) (fast-fp//error n-k k)]
                         [(b-hi b-lo) (fast-fp//error n n-k)])
             (fpround
              (* (fpexp (- (fpstirling n) (fpstirling k) (fpstirling n-k)))
                 (fpsqrt (/ (/ (/ n k) n-k) (fp* 2.0 pi)))
                 (fpexpt+ a-hi a-lo k)
                 (fpexpt+ b-hi b-lo n))))]))

  (: fplog-binomial (float float -> float))
  ;; Error <= 2 ulps
  (define (fplog-binomial n k)
    (cond [(not (integer? n))  +nan.0]
          [(not (integer? k))  +nan.0]
          [(fp< n 0.0)   +nan.0]
          [(fp<= k 0.0)  (if (fp= k 0.0) 0.0 +nan.0)]
          [(fp>= k n)    (if (fp= k n) 0.0 -inf.0)]
          [(fp> k (fp/ n 2.0))  (fplog-binomial n (fp- n k))]
          [else
           (let* ([n-k (- n k)]
                  [a (* k (fplog (/ n-k k)))]
                  [b (* n (fplog1p (/ k n-k)))])
             (cond [(< (+ a b) (fplog 1e300))  (fplog (fpbinomial n k))]
                   [else
                    (+ (- (fpstirling n) (fpstirling k) (fpstirling n-k))
                       (* 0.5 (fplog (/ (/ (/ n k) n-k) (fp* 2.0 pi))))
                       a b)]))]))

  ;; ===================================================================================================
  ;; Permutations

  (: fppermutations-stirling (float float -> float))
  (define (fppermutations-stirling n k)
    (define-values (a-hi a-lo) (fast-fp//error (+ n 1.0) (+ n (- 1.0 k))))
    (* (fpexp (- k))
       (fpexpt+ n (- 1.0 k) k)
       (fpexpt+ a-hi a-lo n)
       (fpexpt+ a-hi a-lo 0.5)
       (fpexp (- (fpstirling (+ n 1.0))
                 (fpstirling (+ n (- 1.0 k)))))))

  (: fppermutations (float float -> float))
  ;; Error <= 3 ulps
  (define (fppermutations n k)
    (cond [(not (integer? n))  +nan.0]
          [(not (integer? k))  +nan.0]
          [(fp< n 0.0)   +nan.0]
          [(fp<= k 0.0)  (if (fp= k 0.0) 1.0 +nan.0)]
          [(fp> k n)  0.0]  ; also handles n = 0 case
          [(fp> k 171.0)  +inf.0]
          [(fp< n 171.0)  (fpround (/ (fpfactorial n) (fpfactorial (- n k))))]
          [(fp< n 9e15)  (fpround (fppermutations-stirling n k))]
          [(fp> k 19.0)  +inf.0]
          [else
           ;; Adding 1.0 to `n' no longer changes it; switch to exact for this
           ;; There's probably a faster way...
           (let loop ([z 1] [n  (fp->exact-integer n)] [k  (fp->exact-integer k)])
             (cond [(> k 0)  (loop (* z n) (- n 1) (- k 1))]
                   [else  (fp z)]))]))

  (: fplog-permutations (float float -> float))
  ;; Error <= 2 ulps
  (define (fplog-permutations n k)
    (cond [(not (integer? n))  +nan.0]
          [(not (integer? k))  +nan.0]
          [(fp< n 0.0)  +nan.0]
          [(fp<= k 0.0)  (if (fp= k 0.0) 0.0 +nan.0)]
          [(fp> k n)  -inf.0]  ; also handles n = 0 case
          [(fp< n 171.0)  (fplog (fpround (fp/ (fp (factorial (fp->fx n)))
                                               (fp (factorial (fp->fx (- n k)))))))]
          [else
           (let* ([n-k (fp- n k)]
                  [a (fp* (fp+ n 0.5) (fplog1p (fp/ k (fp+ n (fp- 1.0 k)))))]
                  [b (fp* k (fp- (fplog1p n-k) 1.0))])
             (cond [(fp<= (fp+ a b) (fplog 1e300))  (fplog (fppermutations n k))]
                   [else  (+ (fp- (fpstirling (fp+ n 1.0)) (fpstirling (fp+ n (fp- 1.0 k))))
                             a b)]))]))

  ;; ===================================================================================================
  ;; Multinomial

  (: fllog-multinomial (float (list-of float) -> float))
  (define (fplog-multinomial n ks)
    (cond [(< n 0)  +nan.0]
          [(ormap negative? ks)  +nan.0]
          [(not (= n (apply + ks)))  -inf.0]
          [(ormap (lambda (k) (assume ((k float)) (= n k))) ks)  0.0]
          [else  (apply - (fplog-factorial n) (map fplog-factorial ks))]))

  (: fpmultinomial (float (list-of float) -> float))
  (define (fpmultinomial n ks)
    (fpexp (fplog-multinomial n ks))))
