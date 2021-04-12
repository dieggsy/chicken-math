(module math.flonum.log (fplog1p
                         fplog+
                         lg1+ lg+ lg1- lg- lgsum
                         fplog-quotient
                         fplog2
                         fplogb)
  (import scheme
          chicken.type
          chicken.fixnum
          (only chicken.base let-values)
          math.flonum.functions
          math.flonum.constants
          math.flonum.exp
          math.flonum.error
          math.flonum.fpvector
          math.flonum.polyfun
          math.flonum.expansion.base
          math.flonum.expansion.exp-reduction)

  (: fplog1p (float -> float))
  ;; Computes the value of log(1+x) in a way that is accurate for small x
  (define fplog1p
    (let ([epsilon.0/2 (fp* 0.5 epsilon.0)])
      ;; approximation for log1p around 0.
      ;; don't put the first term (1.0) in the poly since too much accuracy will be lost
      (define taylor-0-to-1e-3
        (let ([taylor (make-fppolyfun (#i-1/2 #i1/3 #i-1/4 #i1/5 #i-1/6 #i1/7))])
          (lambda (x) (fp+ x (fp* x (fp* x (taylor x)))))))
      (define taylor-0-to-1e-1
        (let ([taylor (make-fppolyfun (#i-1/2 #i1/3 #i-1/4 #i1/5 #i-1/6 #i1/7 #i-1/8 #i1/9
                                              #i-1/10 #i1/11 #i-1/12 #i1/13 #i-1/14 #i1/15 #i-1/16))])
          (lambda (x) (fp+ x (fp* x (fp* x (taylor x)))))))

      (define (normal x)
        (define y (fp+ 1.0 x))
        (fp- (fplog y) (fp/ (fp- (fp- y 1.0) x) y)))

      ;; remap (log1p x) to (+ (log n) (log1p (/ (- x (- n 1)) n)))
      ;; f2/f1 = (fl2 (bigfloat->real (bflog (bf n))))
      (define (make-remap n f2 f1)
        (define m (fp- n 1.0))
        (lambda (x)
          (define x* (fp/ (fp- x m) n))
          (define ax* (fpabs x*))
          (define y (if (fp<= ax* epsilon.0/2)
                        x*
                        (if (fp<= ax* 1e-3)
                            (taylor-0-to-1e-3 x*)
                            (taylor-0-to-1e-1 x*))))
          (fp+ f2 (fp+ y f1))))

      (define remap-0 (make-remap 0.88671875 -0.1202274269981598   2.8375497328444e-18))
      (define remap-1 (make-remap 0.828125   -0.18859116980755003  7.432164219196925e-18))
      (define remap-2 (make-remap 0.7734375  -0.2569104137850272  -2.502843296152504e-17))
      (define remap-3 (make-remap 0.69140625 -0.36902771190573336  2.4362468710901017e-17))
      (define remap-4 (make-remap 0.625      -0.4700036292457356   2.3229412495470032e-17))

      (define remap+0 (make-remap 1.125  0.11778303565638346 -1.1971685747593677e-18))
      (define remap+1 (make-remap 1.1875 0.17185025692665923 -6.0224538210113705e-18))
      (define remap+2 (make-remap 1.25   0.22314355131420976 -9.091270597324799e-18))
      (define remap+3 (make-remap 1.375  0.3184537311185346   2.7114779367326236e-17))
      (define remap+4 (make-remap 1.5    0.4054651081081644  -2.8811380259626426e-18))
      (define remap+5 (make-remap 1.75   0.5596157879354227   2.685492580212308e-17))
      (define remap+6 (make-remap 2.0    0.6931471805599453   2.3190468138462996e-17))
      (define remap+7 (make-remap 2.5    0.9162907318741551  -4.141195369011963e-17))
      (define remap+8 (make-remap 3.0    1.0986122886681098  -9.07129723500153e-17))
      (define remap+9 (make-remap 3.5    1.252762968495368   -6.097690852192957e-17))
      (define remap+a (make-remap 4.4375 1.490091154801534    9.349202385763595e-17))

      (lambda (x)
        (define ax (fpabs x))
        (cond
         [(fp>= ax 4.0)  (fplog (fp+ 1.0 x))] ;; <= change boundary
         [(fp<= ax epsilon.0/2) x]
         [(fp<= ax 1e-3) (taylor-0-to-1e-3 x)]
         [(fp<= ax 1e-1) (taylor-0-to-1e-1 x)]
         [(fp=  x  -1.0) -inf.0]

         [(fp<= x -0.45)      (normal x)]  ; map at / upper limit
         [(fp<= x -0.3515625) (remap-4 x)] ; #x0.a0 / #x-0.5a
         [(fp<= x -0.2734375) (remap-3 x)] ; #x0.b1 / #x-0.46
         [(fp<= x -0.203125)  (remap-2 x)] ; #x0.c6 / #x-0.34
         [(fp<= x -0.140625)  (remap-1 x)] ; #x0.d4 / #x-0.24
         [(fp<= x  0.0)       (remap-0 x)] ; #x0.e3 / _

         [(fp<= x 0.15625) (remap+0 x)] ; #x1.2 / #x0.28
         [(fp<= x 0.21875) (remap+1 x)] ; #x1.3 / #x0.38
         [(fp<= x 0.3125)  (remap+2 x)] ; #x1.4 / #x0.5
         [(fp<= x 0.4375)  (remap+3 x)] ; #x1.6 / #x0.7
         [(fp<= x 0.625)   (remap+4 x)] ; #x1.8 / #x0.a
         [(fp<= x 0.875)   (remap+5 x)] ; #x1.c / #x0.e
         [(fp<= x 1.25)    (remap+6 x)] ; #x2.0 / #x1.4
         [(fp<= x 1.75)    (remap+7 x)] ; #x2.8 / #x1.c
         [(fp<= x 2.25)    (remap+8 x)] ; #x3.0 / #x2.4
         [(fp<= x 2.9375)  (remap+9 x)] ; #x3.8 / #2.f0
         [else             (remap+a x)] ; #x4.4 / #4.0
         ))))

  (: fplog+ (float float -> float))
  ;; Computes log(a+b) in a way that is accurate for a+b near 1.0
  (define (fplog+ a b)
    (define a+b (+ a b))
    (cond [(< (fpabs (- a+b 1.0)) (fplog 2.0))
           ;; a+b is too close to 1.0, so compute in higher precision
           (let-values (((a+b a+b-lo) (fast-fp+/error a b)))
             (- (fplog a+b) (fplog1p (- (/ a+b-lo a+b)))))]
          [(= a+b +inf.0)
           ;; a+b overflowed, so reduce the arguments
           (+ (fplog 2.0) (fplog (+ (* 0.5 a) (* 0.5 b))))]
          [else
           (fplog a+b)]))

  (: lg1+ (float -> float))
  (define (lg1+ log-x)
    (cond [(fp>= log-x 0.0)  (fp+ log-x (fplog1p (fpexp (- log-x))))]
          [else  (fplog1p (fpexp log-x))]))

  (: lg+ (float float -> float))
  (define (lg+ log-x log-y)
    (let ([log-x  (fpmax log-x log-y)]
          [log-y  (fpmin log-x log-y)])
      (cond [(fp= log-x -inf.0)  -inf.0]
            [else  (fp+ log-x (fplog1p (fpexp (fp- log-y log-x))))])))

  (: lg1- (float -> float))
  (define (lg1- log-x)
    (cond [(fp> log-x (fplog 0.5))  (fplog (- (fpexpm1 log-x)))]
          [else  (fplog1p (- (fpexp log-x)))]))

  (: lg- (float float -> float))
  (define (lg- log-x log-y)
    (cond [(fp< log-x log-y)  +nan.0]
          [(fp= log-x -inf.0)  -inf.0]
          [else  (fp+ log-x (lg1- (fp- log-y log-x)))]))

  (: fpmax* ((list-of float) -> float))
  (define (fpmax* xs)
    (let loop ([xs xs] [mx -inf.0])
      (if (null? xs) mx (loop (cdr xs) (fpmax mx (car xs))))))

  (: lgsum ((list-of float) -> float))
  (define (lgsum log-xs)
    (if (null? log-xs)
        0.0
        (let ([log-x0  (car log-xs)]
              [log-xs  (cdr log-xs)])
          (if (null? log-xs)
              log-x0
              (let ([log-x1  (car log-xs)]
                    [log-xs  (cdr log-xs)])
                (if (null? log-xs)
                    (lg+ log-x0 log-x1)
                    (let ([max-log-x  (fpmax (fpmax log-x0 log-x1) (fpmax* log-xs))])
                      (if (fp= max-log-x -inf.0)
                          -inf.0
                          (let ([s  (fpsum
                                     (apply list -1.0  ; for the max element; faster than removing it
                                            (fpexp (- log-x0 max-log-x))
                                            (fpexp (- log-x1 max-log-x))
                                            (map (lambda (log-x) (assume ((log-x float)) (fpexp (- log-x max-log-x))))
                                                 log-xs)))])
                            ;; Yes, we subtract 1.0 and then add 1.0 before taking the log; this
                            ;; helps with precision a bit when s is near zero
                            (+ max-log-x (fplog1p s)))))))))))

  (: fplog-quotient (float float -> float))
  ;; Computes (fllog (/ x y)) in a way that reduces error and avoids under-/overflow
  (define (fplog-quotient x y)
    (let ([x  (fpabs x)]
          [y  (fpabs y)]
          [s  (fp/ (fpsgn x) (fpsgn y))])
      (cond [(fp> s 0.0)
             (let ((z (fp/ x y)))
              (cond [(and (fp> z +max-subnormal.0) (fp< z +inf.0))  (fplog (fp* s z))]
                   [else  (fp+ (fplog x) (- (fplog y)))]))]
            [(fp= s 0.0)  -inf.0]
            [else  +nan.0])))

  (define log-max.0 (fplog +max.0))
  (define log2.0 (fplog 2.0))

  (: fplog2* (float -> float))
  ;; Computes log2(x) with a least 8 extra bits precision, which reduces the probability of rounding
  ;; error significantly. Assumes 0.0 < x < +inf.0 and x != 1.0.
  (define (fplog2* x)
    (let* ([log-x  (fplog x)]
           ;; Solve for x^(2^k) = +max.0 (k is basically the number of extra bits precision)
           [k  (fp/ (fplog (fp/ log-max.0 (fpabs log-x))) log2.0)]
           ;; We'll be operating on x^adj, which is huge
           [adj  (fpexp2 (fpceiling (- k 1.0)))]
           [adj  (if (fp>= x 1.0) adj (- adj))]
           ;; Compute fpoor(log2(x^adj))
           [y2  (fptruncate (fp/ (fp* adj log-x) log2.0))]
           ;; Compute "remainder" log2(x^adj/2^y2) (note: dividing by 2^y2 is exact)
           [y1  (fp/ (fplog (fp/ (fpexpt x adj) (fpexp2 y2))) log2.0)])
      (fp+ (fp/ y2 adj) (fp/ y1 adj))))

  (: fplog2 (float -> float))
  ;; Largest observed error is 0.5006 ulps
  (define (fplog2 x)
    (cond [(fp<= x 0.0)  (if (fp< x 0.0) +nan.0 -inf.0)]
          [(fp< x +inf.0)  (if (fp= x 1.0) 0.0 (fplog2* x))]
          [(fp= x +inf.0)  +inf.0]
          [else  +nan.0]))

  (: fplogb (float float -> float))
  ;; Largest observed error is 2.1 ulps, but is usually < 0.7 ulps
  (define (fplogb b x)
    (cond [(fp= x 1.0)  0.0]
          [(fp= b 1.0)
           ;; For x != 1, first limit wrt x: +inf.0 or -inf.0
           +nan.0]
          [(fp= b 2.0)
           ;; Using the more accurate `fplog2' ensures that exact cases have zero error
           (fplog2 x)]
          [(not (and (fp<= 0.0 b) (fp<= b +inf.0) (fp<= 0.0 x) (fp<= x +inf.0)))
           ;; One or both is out of bounds or is +nan.0
           +nan.0]
          [(fp= b 0.0)
           (cond [(fp= x 0.0)
                  ;; First limit wrt x: +inf.0
                  ;; First limit wrt b: 0.0
                  ;; +inf.0 corrects left-inverse case (fplogb 0.0 (fpexpt 0.0 +inf.0))
                  ;; +inf.0 corrects right-inverse case (fpexpt 0.0 (fplogb 0.0 0.0))
                  +inf.0]
                 [(fp= x +inf.0)
                  ;; First limit wrt x: -inf.0
                  ;; First limit wrt b: 0.0
                  ;; -inf.0 corrects left-inverse case (fplogb 0.0 (fpexpt 0.0 -inf.0))
                  ;; -inf.0 corrects right-inverse case (fpexpt 0.0 (fplogb 0.0 +inf.0))
                  -inf.0]
                 [(fp<= x 1.0)  0.0]
                 [else  -0.0])]
          [(fp= b +inf.0)
           (cond [(fp= x 0.0)
                  ;; First limit wrt x: -inf.0
                  ;; First limit wrt b: -0.0
                  ;; -inf.0 corrects left-inverse case (fplogb +inf.0 (fpexpt +inf.0 -inf.0))
                  ;; -inf.0 corrects right-inverse case (fpexpt +inf.0 (fplogb +inf.0 0.0))
                  -inf.0]
                 [(fp= x +inf.0)
                  ;; First limit wrt x: +inf.0
                  ;; First limit wrt b: 0.0
                  ;; +inf.0 corrects left-inverse case (fplogb +inf.0 (fpexpt +inf.0 +inf.0))
                  ;; +inf.0 corrects right-inverse case (fpexpt +inf.0 (fplogb +inf.0 +inf.0))
                  +inf.0]
                 [(fp<= 1.0 x)  0.0]
                 [else  -0.0])]
          [(fp= x 0.0)  (if (fp< b 1.0) +inf.0 -inf.0)]
          [(fp= x +inf.0)  (if (fp< b 1.0) -inf.0 +inf.0)]
          [else
           (let* ((log-b (fplog b))
                  (y (fp/ (fplog x) log-b))
                  ;; One Newton iteration reduces error to <= 1 ulp (instead of <= 2 ulps)
                  (numer (fp- x (fpexpt b y)))
                  (denom (fp* x log-b)))
             (cond [(and (fp> numer -inf.0) (fp< numer +inf.0)
                         (fp> denom 0.0) (fp< denom +inf.0))
                    (fp+ y (fp/ numer denom))]
                   [else
                    ;; Oh noes! We had overfpows or underfpows!
                    ;; Not a lot we can do without introducing more error, so just return y
                    y]))]))

  )
