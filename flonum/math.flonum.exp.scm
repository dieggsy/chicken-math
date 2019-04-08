(module math.flonum.exp (fpexpm1
                         fpexpsqr
                         fpgauss
                         fpexp1p
                         fpexp2
                         fppow2near)

  (import scheme
          chicken.type
          (only srfi-1 list-tabulate)
          math.flonum.functions
          math.flonum.constants
          math.flonum.polyfun)

  (define (build-vector len fn)
    (list->vector (list-tabulate len fn)))

  (define expm1-poly-numer
    (make-fppolyfun
     (-0.28127670288085937e-1
      0.51278186299064534e0
      -0.6310029069350198e-1
      0.11638457975729296e-1
      -0.52143390687521003e-3
      0.21491399776965688e-4)))

  (define expm1-poly-denom
    (make-fppolyfun
     ( 1.0
       -0.45442309511354755e0
       0.90850389570911714e-1
       -0.10088963629815502e-1
       0.63003407478692265e-3
       -0.17976570003654402e-4)))

  (: fpexpm1/poly (float -> float))
  (define (fpexpm1/poly x)
    ;; Define negative in terms of positive to avoid cancellation error
    (cond [(fp< x 0.0)
           (let ((y (fpexpm1/poly (- x))))
             (fp/ (- y) (fp+ y 1.0)))]
          [else  (fp+ (fp* x 0.10281276702880859e1)
                      (fp* x (fp/ (expm1-poly-numer x) (expm1-poly-denom x))))]))

  ;; Integer arguments for flexp2
  (: fpexp2s (or false (vector-of float)))
  (define fpexp2s #f)

  (define (build-fpexp2s)
    (: new-fpexp2s (vector-of float))
    (define new-fpexp2s (build-vector (- 1024 -1074) (lambda (n) (fp (expt 2 (- n 1074))))))
    (set! fpexp2s new-fpexp2s)
    new-fpexp2s)

  (: flexpm1 (float -> float))
  ;; Computes exp(x)-1 in a way that is accurate for small x
  (define (fpexpm1 x)
    (define ax (fpabs x))
    (cond [(fp>= ax 0.5)  (fp- (fpexp x) 1.0)]
          [(fp> ax (fp* 0.5 epsilon.0))  (fpexpm1/poly x)]
          [else  x]))

  (: fpgauss (float -> float))
  ;; Computes exp(-x^2) in a way that is accurate for large x
  (define (fpgauss x)
    (let ([x  (fpabs x)])
      (cond [(fp> x 28.0)  0.0]
            [(fp> x 1.0)
             ;; Split x into a fponum with 21 high-order fractional bits and a part with the rest
             ;; Sometime after p > 26.1, (exp (- (* p p))) outputs subnormals, so we don't go there
             (let* ((p (fpmin 26.0 (fp/ (fptruncate (fp* (fpexpt 2.0 21.0) x)) (fpexpt 2.0 21.0))))
                    (q (fp- x p)))
               (fp* (fp* (fpexp (- (fp* 2.0 (fp* p q))))
                         (fpexp (- (fp* q q))))
                    (fpexp (- (fp* p p)))))]
            [else
             (fpexp (- (fp* x x)))])))

  (: fpexpsqr (float -> float))
  ;; Computes exp(x^2) in a way that is accurate for large x
  (define (fpexpsqr x)
    (let ([x  (fpabs x)])
      (cond [(fp> x 27.0)  +inf.0]
            [(fp> x 1.0)
             (let* ((p (fp/ (fptruncate (fp* (fpexpt 2.0 21.0) x)) (fpexpt 2.0 21.0)))
                    (q (fp- x p)))
               (fp* (fp* (fpexp (fp* 2.0 (fp* p q)))
                         (fpexp (fp* q q)))
                    (fpexp (fp* p p))))]
            [else
             (fpexp (fp* x x))])))

  (: fpexp1p (float -> float))
  ;; Computes exp(1+x) in a way that is accurate near powers of 2
  (define (fpexp1p x)
    (cond [(fp< x -0.5)  (fpexp (fp+ 1.0 x))]
          [else
           (let* ((lg2x (fpfloor (fp/ (fplog x) (fplog 2.0))))
                  (lg2x+1 (fpfloor (fp/ (fplog (fp+ 1.0 x)) (fplog 2.0)))))
             (cond [(fp= lg2x lg2x+1)  (fpexp (fp+ 1.0 x))]
                   [else  (fp* (fpexp x) (fpexp 1.0))]))]))

  (: fpexp2 (float -> float))
  ;; Computes 2^x
  (define (fpexp2 x)
    (cond [(fp<= x -1075.0)  0.0]
          [(fp>= x 1024.0)  +inf.0]
          [(fp= x (fpround x))
           (let* ([fpexp2s  fpexp2s]
                  [fpexp2s  (if fpexp2s fpexp2s (build-fpexp2s))])
             (vector-ref fpexp2s (inexact->exact (fp+ x 1074.0))))]
          [else  (fpexpt 2.0 x)]))

  (: fppow2near (float -> float))
  (define (fppow2near a)
    (fpexp2 (fpmax -1074.0 (fpmin 1023.0 (fpround (fp/ (fplog (fpabs a)) (fplog 2.0))))))))
