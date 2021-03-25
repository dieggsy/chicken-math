(module math.flonum.functions (fp
                               fpsubnormal? fprational? fpinfinite? fpnan? fpinteger?
                               fpnext* fpprev*
                               fpulp-error
                               fpeven? fpodd? fpsgn fphypot fplog/base
                               fpprobability?
                               fpsinpix fpcospix fptanpix fpcscpix fpsecpix fpcotpix)
  (import scheme
          chicken.type
          chicken.flonum
          chicken.module
          (only chicken.base let*-values)
          (only chicken.foreign foreign-declare foreign-value)
          math.flonum.constants
          math.flonum.bits
          math.racket-shim)

  (reexport chicken.flonum)

  (: fpsubnormal? (float -> boolean))
  (define (fpsubnormal? x)
    (and (fp<= (fpabs x) +max-subnormal.0)
         (not (fp= x 0.0))))

  (: fprational? (float -> boolean))
  (define (fprational? x)
    (fp< (fpabs x) +inf.0))

  (: fpinfinite? (float -> boolean))
  (define (fpinfinite? x)
    (fp= (fpabs x) +inf.0))

  (: fpnan? (float -> boolean))
  (define (fpnan? x)
    (not (fp<= (fpabs x) +inf.0)))

  ;; (: fpinteger? (float -> boolean))
  ;; (define (fpinteger? x)
  ;;   (fp= x (fptruncate x)))

  (: fpsubnormal-next* (float -> float))
  (define (fpsubnormal-next* x)
    (fp/ (fp+ (fp* x (fpexpt 2.0 1022.0)) epsilon.0)
         (fpexpt 2.0 1022.0)))

  (: fpsubnormal-prev* (float -> float))
  (define (fpsubnormal-prev* x)
    (fp/ (fp- (fp* x (fpexpt 2.0 1022.0)) epsilon.0)
         (fpexpt 2.0 1022.0)))

  (: fpnext* (float -> float))
  (define (fpnext* x)
    (cond [(fp< x 0.0)  (fp- 0.0 (fpprev* (fp- 0.0 x)))]
          [(fp= x 0.0)  +min.0]
          [(fp= x +inf.0)  +inf.0]
          [else  (let ((next-x (fp+ x (fp* x (fp* 0.5 epsilon.0)))))
                   (cond [(fp= next-x x)  (fp+ x (fp* x epsilon.0))]
                         [else  next-x]))]))

  (: fpprev* (float -> float))
  (define (fpprev* x)
    (cond [(fp< x 0.0)  (fp- 0.0 (fpnext* (fp- 0.0 x)))]
          [(fp= x 0.0)  -min.0]
          [(fp= x +inf.0)  +max.0]
          [else  (let ((prev-x (fp- x (fp* x (fp* 0.5 epsilon.0)))))
                   (cond [(fp= prev-x x)  (fp- x (fp* x epsilon.0))]
                         [else  prev-x]))]))
  ;; ==========================================================================
  ;; Error measurement

  (: fpulp-error (float number -> float))
  (define (fpulp-error x r)
    (define r.0 (fp r))
    (cond [(eqv? x r)  0.0]
          [(and (fp= x 0.0) (fp= r.0 0.0))  0.0]
          [(and (fp= x +inf.0) (fp= r.0 +inf.0))  0.0]
          [(and (fp= x -inf.0) (fp= r.0 -inf.0))  0.0]
          [(zero? r)  +inf.0]
          [(and (fprational? x) (fprational? r.0))
           (fpabs (fp (/ (- (inexact->exact x) (inexact->exact r))
                         (inexact->exact (fpmax +min.0 (fpulp r.0))))))]
          [else  +inf.0]))

  ;; ==========================================================================
  ;; More floating-point functions

  (: fpsgn (float -> float))
  (define (fpsgn x)
    (cond [(fp< x 0.0) -1.0]
          [(fp< 0.0 x)  1.0]
          [else  0.0]))

  (: fpeven? (float -> boolean))
  (define (fpeven? x)
    (let ([x  (fpabs x)])
      (or (fp= x 0.0)
          (and (fp>= x 2.0)
               (let ([0.5x  (fp* 0.5 x)])
                 (fp= (truncate 0.5x) 0.5x))))))

  (define last-odd (fp- (fpexpt 2.0 53.0) 1.0))

  (: fpodd? (float -> boolean))
  (define (fpodd? x)
    (let ([x  (fpabs x)])
      (and (fp>= x 1.0) (fp<= x last-odd)
           (let ([0.5x  (fp* 0.5 (fp+ 1.0 x))])
             (fp= (truncate 0.5x) 0.5x)))))

  (: fphypot (float float -> float))
  (define (fphypot x y)
    (define xa (fpabs x))
    (define ya (fpabs y))
    (let ([xa  (fpmin xa ya)]
          [ya  (fpmax xa ya)])
      (cond [(fp= xa 0.0)  ya]
            [(fprational? ya)
             (let ((u (fp/ xa ya)))
               (fp* ya (fpsqrt (fp+ 1.0 (fp* u u)))))]
            [else ya])))

  ;; todo: overflow not likely; underflow likely
  (: fplog/base (float float -> float))
  (define (fplog/base b x)
    (fp/ (fplog x) (fplog b)))

  (: fpprobability? (float #!optional boolean -> boolean))
  (define (fpprobability? p #!optional (log? #f))
    (cond [log?  (and (fp>= p -inf.0) (fp<= p 0.0))]
          [else  (and (fp>= p 0.0) (fp<= p 1.0))]))

  (: fpsinpix (float -> float))
  ;; Computes sin(pi*x) accurately; i.e. error <= 2 ulps but almost always <= 1 ulp
  (define (fpsinpix x)
    (cond [(fp= x 0.0)  x]
          [(and (fp> x -inf.0) (fp< x +inf.0))
           (let*-values
               ([(x s)  (if (fp< x 0.0) (values (- x) -1.0) (values x 1.0))]
                [(x)    (fp- x (fp* 2.0 (fptruncate (fp* 0.5 x))))]
                [(x s)  (if (fp> x 1.0) (values (fp- x 1.0) (fp* s -1.0)) (values x s))]
                [(x)    (if (fp> x 0.5) (fp- 1.0 x) x)])
             (fp* s (fpsin (fp* pi x))))]
          [else  +nan.0]))

  (: fpcospix (float -> float))
  ;; Computes cos(pi*x) accurately; i.e. error <= 1 ulps
  (define (fpcospix x)
    (cond [(and (fp> x -inf.0) (fp< x +inf.0))
           (let*-values
               ([(x)  (fpabs x)]
                [(x)  (fp- x (fp* 2.0 (fptruncate (fp* 0.5 x))))]
                [(x)  (if (fp> x 1.0) (fp- 2.0 x) x)]
                [(x s)  (if (fp> x 0.5) (values (fp- 1.0 x) -1.0) (values x 1.0))])
             (cond [(fp> x 0.25)  (fp* (fp* s -1.0) (fpsin (fp* pi (fp- x 0.5))))]
                   [else  (fp* s (fpcos (fp* pi x)))]))]
          [else  +nan.0]))

  (: fptanpix (float -> float))
  ;; Computes tan(pi*x) accurately; i.e. error <= 2 ulps but almost always <= 1 ulp
  (define (fptanpix x)
    (cond [(fp= x 0.0)  x]
          [(and (fp> x -inf.0) (fp< x +inf.0))
           (let*-values
               ([(x s)  (if (fp< x 0.0) (values (- x) -1.0) (values x 1.0))]
                [(x)    (fp- x (fptruncate x))]
                [(x s)  (if (fp> x 0.5) (values (fp- 1.0 x) (fp* s -1.0)) (values x s))])
             (cond [(fp= x 0.5)  +nan.0]
                   [(fp> x 0.25)  (fp/ s (fptan (fp* pi (fp- 0.5 x))))]
                   [else  (fp* s (fptan (fp* pi x)))]))]
          [else  +nan.0]))

  (: fpcscpix (float -> float))
  (define (fpcscpix x)
    (cond [(and (not (zero? x)) (fpinteger? x))  +nan.0]
          [else  (/ 1.0 (fpsinpix x))]))

  (: fpsecpix (float -> float))
  (define (fpsecpix x)
    (cond [(and (fp> x 0.0) (fpinteger? (fp- x 0.5)))  +nan.0]
          [(and (fp< x 0.0) (fpinteger? (fp+ x 0.5)))  +nan.0]
          [else  (/ 1.0 (fpcospix x))]))

  (: fpcotpix (float -> float))
  ;; Computes 1/tan(pi*x) accurately; i.e. error <= 2 ulps but almost always <= 1 ulp
  (define (fpcotpix x)
    (cond [(fp= x 0.0)  (fp/ 1.0 x)]
          [(and (fp> x -inf.0) (fp< x +inf.0))
           (let*-values
               ([(x s)  (if (fp< x 0.0) (values (- x) -1.0) (values x 1.0))]
                [(x)    (fp- x (fptruncate x))]
                [(x s)  (if (fp> x 0.5) (values (fp- 1.0 x) (fp* s -1.0)) (values x s))])
             (cond [(fp= x 0.0)  +nan.0]
                   [(fp< x 0.25)  (fp/ s (fptan (fp* pi x)))]
                   [else  (fp* s (fptan (fp* pi (fp- 0.5 x))))]))]
          [else  +nan.0])))
