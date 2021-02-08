(module math.polynomial.chebyshev (chebyshev-poly
                                   chebyshev-poly?
                                   chebyshev-poly-min
                                   chebyshev-poly-max
                                   chebyshev-poly-coefs

                                   chebyshev-poly-order
                                   chebyshev-poly-convert

                                   build-chebyshev-poly
                                   build-chebyshev-fppoly
                                   ;; build-chebyshev-bfpoly

                                   chebyshev-poly-fun
                                   chebyshev-fppoly-fun
                                   ;; chebyshev-bfpoly-fun

                                   inline-chebyshev-fppoly-fun
                                   chebyshev-iter)
  (import scheme
          chicken.base
          chicken.type
          chicken.flonum
          srfi-133
          miscmacros
          math.base
          ;; math.racket.bigfloat
          math.flonum.functions
          math.racket-shim)

  (define-type chebyshev-poly (struct chebyshev-poly))

  (define-record chebyshev-poly
    min
    max
    coefs)

  (define chebyshev-poly make-chebyshev-poly)

  (: chebyshev-poly-order (chebyshev-poly -> number))
  (define (chebyshev-poly-order p)
    (define n (vector-length (chebyshev-poly-coefs p)))
    (if (zero? n) 0 (- n 1)))


  (: chebyshev-poly-convert (chebyshev-poly (any -> any) -> chebyshev-poly))
  (define (chebyshev-poly-convert p conv)
    (define mn (chebyshev-poly-min p))
    (define mx (chebyshev-poly-max p))
    (define cs (chebyshev-poly-coefs p))
    (chebyshev-poly (conv mn) (conv mx) (vector-map conv cs)))

  ;; (: make-build-chebyshev-poly
  ;;    (All (A) ((A A -> A) (A A -> A) (A A -> A) (A A -> A) (A -> A) (Integer -> A) (-> A)
  ;;              -> (A A Integer (A -> A) -> (chebyshev-poly A)))))
  (define-syntax-rule (make-build-chebyshev-poly num+ num- num* num/ numcos int->num pi-thnk)
    (lambda (mn mx n f)
      (define 0.num (int->num 0))
      (define 1.num (int->num 1))
      (define 2.num (int->num 2))
      (define 1/2.num (num/ 1.num 2.num))
      (define pi.num (pi-thnk))
      (define mx-mn (num- mx mn))
      (define mn+mx (num+ mn mx))
      (let ([n.num  (int->num n)])
        (define norm (num/ 2.num n.num))
        (define cs
          (let ((vec (make-vector n)))
            (let loop ((j 0))
              (unless (= j n)
                (vector-set! vec j
                             (let ([j  (int->num j)])
                               (num*
                                (let loop ((cj 0.num)
                                           (k 0))
                                  (if (= k n)
                                      cj
                                      (let* ((theta (num/ (num* pi.num (num+ (int->num k) 1/2.num)) n.num))
                                             (x (num* 1/2.num (num+ (num* mx-mn (numcos theta)) mn+mx))))
                                        (loop (add1 k)
                                              (num+ cj (num* (f x) (numcos (num* theta j))))))))
                                norm)))
                (loop (add1 j))))))
        (chebyshev-poly mn mx cs))))

  (define-syntax-rule (make-chebyshev-poly-fun num+ num- num* num/ int->num)
    (lambda (p)
      (define cs (chebyshev-poly-coefs p))
      (define n (vector-length cs))
      (define 0.num (int->num 0))
      (define 2.num (int->num 2))
      (cond
       [(zero? n)  (lambda (x) 0.num)]
       [else
        (let ((mn (chebyshev-poly-min p))
              (mx (chebyshev-poly-max p)))
          (lambda (x)
            (define i (- n 1))
            (define c (vector-ref cs i))
            (define y (num/ (num- (num* x 2.num) (num+ mn mx))
                            (num- mx mn)))
            (define y2 (num* y 2.num))
            (let loop ([i i]
                       [c c]
                       [d 0.num]
                       [dd 0.num])
              (cond [(zero? i)  (num+ (num* y d) (num- (num/ c 2.num) dd))]
                    [else
                     (let ([d   (num+ (num* y2 d) (num- c dd))]
                           [dd  d])
                       (loop (- i 1) (vector-ref cs (- i 1)) d dd))]))))])))

  (define build-chebyshev-poly
    (make-build-chebyshev-poly + - * / cos (lambda (x) x) (lambda () pi)))

  (define build-chebyshev-fppoly
    (make-build-chebyshev-poly fp+ fp- fp* fp/ fpcos fp (lambda () pi)))

  ;; (define build-chebyshev-bfpoly
  ;;   (make-build-chebyshev-poly Bigfloat bf+ bf- bf* bf/ bfcos bf (lambda () pi.bf)))

  (define chebyshev-poly-fun
    (make-chebyshev-poly-fun + - * / (lambda (x) x)))

  (define chebyshev-fppoly-fun
    (make-chebyshev-poly-fun fp+ fp- fp* fp/ fp))

  ;; (define chebyshev-bfpoly-fun
  ;;   (make-chebyshev-poly-fun Bigfloat bf+ bf- bf* bf/ bf))

  (define-syntax chebyshev-iter
    (syntax-rules ()
      [(_ y y2 d dd ())  d]
      [(_ y y2 d dd (c0))
       (fp+ (fp* y d) (fp- (fp/ c0 2.0) dd))]
      [(_ y y2 d dd (c0 c ...))
       (let ([d   (fp+ (fp* y2 d) (fp- c0 dd))]
             [dd  d])
         (chebyshev-iter y y2 d dd (c ...)))]))

  (define-syntax inline-chebyshev-fppoly-fun
    (er-macro-transformer
     (lambda (x r c)
       (let ((lower (cadr x))
             (upper (caddr x))
             (lst (reverse (cadddr x))))
         `(lambda (z)
            (define y (fp/ (fp- (fp+ z z) (fp+ ,lower ,upper))
                           (fp- ,upper ,lower)))
            (define y2 (fp+ y y))
            (let ([d   0.0]
                  [dd  0.0])
              (chebyshev-iter y y2 d dd ,(cdr lst)))))))))
