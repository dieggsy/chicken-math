(module math.flonum.error (fpsplit
                           fast-mono-fp+/error
                           fast-mono-fp-/error
                           fast-fp+/error
                           fast-fp-/error
                           fast-fp*/error
                           fast-fpsqr/error
                           fast-fp//error
                           fast-fpfma/error
                           fast-fpfsa/error
                           fp+/error
                           fp-/error
                           fp*/error
                           fpsqr/error
                           fp//error
                           fpfma/error
                           fpfsa/error)
  (import scheme
          chicken.type
          (only chicken.base let-values let*-values include-relative)
          (only miscmacros define-syntax-rule)
          math.flonum.functions)

  (include-relative "utils.scm")

  (define-syntax-rule (fpsplit a-expr)
    (let ([a a-expr])
      (let* ([c   (fp* a (fp+ 1.0 (fpexpt 2.0 27.0)))]
             [x2  (fp- c (fp- c a))])
        (values x2 (fp- a x2)))))

  (define-syntax-rule (fast-mono-fp+/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let ([x2  (+ a b)])
        (values x2 (- b (- x2 a))))))

  ;(: fast-mono-fl-/error (float float -> (Values float float)))
  ;; Returns a+b and its rounding error
  ;; Assumes |a| >= |b|
  (define-syntax-rule (fast-mono-fp-/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let ([x2  (- a b)])
        (values x2 (- (- a x2) b)))))

  ;; =================================================================================================
  ;; Fast arithmetic that returns rounding error

  ;(: fast-fl+/error (float float -> (Values float float)))
  ;; Returns a+b and its rounding error
  (define-syntax-rule (fast-fp+/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let* ([x2  (fp+ a b)]
             [v   (fp- x2 a)])
        (values x2 (fp+ (fp- a (fp- x2 v)) (fp- b v))))))

  ;(: fast-fl-/error (float float -> (Values float float)))
  ;; Returns a-b and its rounding error
  (define-syntax-rule (fast-fp-/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let* ([x2  (fp- a b)]
             [v   (fp- x2 a)])
        (values x2 (fp- (fp- a (fp- x2 v)) (fp+ b v))))))

  ;(: fast-fl*/error (float float -> (Values float float)))
  ;; Returns a*b and its rounding error
  (define-syntax-rule (fast-fp*/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let*-values ([(x2)  (fp* a b)]
                    [(a2 a1)  (fpsplit a)]
                    [(b2 b1)  (fpsplit b)])
        (values x2 (- (fp- (fp- (fp- (fp- x2 (fp* a2 b2))
                                     (fp* a1 b2))
                                (fp* a2 b1))
                           (fp* a1 b1)))))))

  ;(: fast-flfma/error (float float float -> (Values float float)))
  ;; Returns a*b+c and its rounding error
  (define-syntax-rule (fast-fpfma/error a-expr b-expr c-expr)
    (let*-values ([(y2 y1)  (fast-fp*/error a-expr b-expr)]
                  [(h0 h1)  (fast-fp+/error c-expr y1)]
                  [(h3 h2)  (fast-fp+/error h0 y2)])
      (values h3 (fp+ h2 h1))))

  ;(: fast-flfsa/error (float float float -> (Values float float)))
  ;; Returns a*a+b and its rounding error
  (define-syntax-rule (fast-fpfsa/error a-expr b-expr)
    (let*-values ([(y2 y1)  (fast-fpsqr/error a-expr)]
                  [(h0 h1)  (fast-fp+/error b-expr y1)]
                  [(h3 h2)  (fast-fp+/error h0 y2)])
      (values h3 (fp+ h2 h1))))

  #;; If we had hardware fused multiply-add:
  (define-syntax-rule (fast-fl*/error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let ([x2  (fl* a b)])
        (values x2 (flfma a b (- x2))))))

  ;(: fast-flsqr/error (float -> (Values float float)))
  ;; Returns a*a and its rounding error
  (define-syntax-rule (fast-fpsqr/error a-expr)
    (let ([a a-expr])
      (let*-values ([(x2)  (fp* a a)]
                    [(a2 a1)  (fpsplit a)])
        (values x2 (- (fp- (fp- (fp- x2 (fp* a2 a2))
                                (fp* 2.0 (fp* a2 a1)))
                           (fp* a1 a1)))))))

  ;(: fast-fl//error (float float -> (Values float float)))
  ;; Returns a/b and its rounding error
  (define-syntax-rule (fast-fp//error a-expr b-expr)
    (let ([a a-expr] [b b-expr])
      (let*-values ([(x2)  (fp/ a b)]
                    [(w2 w1)  (fast-fp*/error x2 b)])
        (fast-mono-fp+/error x2 (fp/ (fp- (fp- a w2) w1) b)))))

  ;; =================================================================================================
  ;; Function versions of the above that are well-defined for the largest domain, and return 0.0 as
  ;; the second argument whenever the first isn't rational


  (: fp+/error (float float -> float float))
  (define (fp+/error a b)
    (let-values ([(x2 x1)  (fast-fp+/error a b)])
      (values x2 (if (fprational? x2) x1 0.0))))

  (: fp-/error (float float -> float float))
  (define (fp-/error a b)
    (let-values ([(x2 x1)  (fast-fp-/error a b)])
      (values x2 (if (fprational? x2) x1 0.0))))

  (: fp*/error (float float -> float float))
  (define (fp*/error a b)
    (let ([x2  (fp* a b)])
      (values x2 (if (and (fprational? x2) (not (fpsubnormal? x2)))
                     (let*-values ([(da db)  (values (near-pow2 a) (near-pow2 b))]
                                   [(d)  (fp* da db)]
                                   [(d?)  (and (fp> d 0.0) (fp< d +inf.0))]
                                   [(a2 a1)  (fpsplit (fp/ a da))]
                                   [(b2 b1)  (fpsplit (fp/ b db))]
                                   [(x2)  (if d? (fp/ x2 d) (fp/ (fp/ x2 da) db))]
                                   [(x1)  (- (fp- (fp- (fp- (fp- x2 (fp* a2 b2))
                                                            (fp* a1 b2))
                                                       (fp* a2 b1))
                                                  (fp* a1 b1)))])
                       (if d? (fp* x1 d) (fp* (fp* x1 da) db)))
                     0.0))))

  (: fpsqr/error (float -> float float))
  (define (fpsqr/error a)
    (let ([x2  (fp* a a)])
      (values x2 (if (and (fprational? x2) (not (fpsubnormal? x2)))
                     (let*-values ([(d)  (near-pow2 a)]
                                   [(d^2)  (fp* d d)]
                                   [(d^2?)  (and (fp> d^2 0.0) (fp< d^2 +inf.0))]
                                   [(a2 a1)  (fpsplit (fp/ a d))]
                                   [(x2)  (if d^2? (fp/ x2 d^2) (fp/ (fp/ x2 d) d))]
                                   [(x1)  (- (fp- (fp- (fp- x2 (fp* a2 a2))
                                                       (fp* 2.0 (fp* a1 a2)))
                                                  (fp* a1 a1)))])
                       (if d^2? (fp* x1 d^2) (fp* (fp* x1 d) d)))
                     0.0))))

  (: fp//error (float float -> float float))
  (define (fp//error a b)
    (let ([x2  (fp/ a b)])
      (values x2 (if (and (fprational? x2) (fprational? b))
                     (let* ([d  (near-pow2/div a b)]
                            [a  (fp/ a d)]
                            [b  (fp/ b d)])
                       (let-values ([(w2 w1)  (fp*/error x2 b)])
                         (fp/ (fp- (fp- a w2) w1) b)))
                     0.0))))

  (: fpfma/error (float float float -> float float))
  (define (fpfma/error a b c)
    (define-values (x2 x1) (fast-fpfma/error a b c))
    (cond [(fprational? (+ x2 x1))  (values x2 x1)]
          [else
           (let* ((n (near-pow2 (max (fpsqrt (abs a)) (fpsqrt (abs b)))))
                 (1/n (/ 1.0 n))
                 (n^2 (* n n)))
             (let-values ([(x2 x1)  (fast-fpfma/error (* a 1/n) (* b 1/n) (* c 1/n 1/n))])
               (values (* n^2 x2) (* n^2 x1))))]))

  (: fpfsa/error (float float -> float float))
  (define (fpfsa/error a b)
    (define-values (x2 x1) (fast-fpfsa/error a b))
    (cond [(fprational? (+ x2 x1))  (values x2 x1)]
          [else
           (let* ((n (near-pow2 (fpsqrt (abs a))))
                  (1/n (/ 1.0 n))
                  (n^2 (* n n)))
             (let-values ([(x2 x1)  (fast-fpfsa/error (* a 1/n) (* b 1/n 1/n))])
               (values (* n^2 x2) (* n^2 x1))))])))
