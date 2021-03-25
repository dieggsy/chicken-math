(test-group "math.flonum"

  (import math.flonum
          math.base
          (only srfi-1 count)
          (only miscmacros repeat))

  (test-group "fpulp-error"

    ;; Both arguments eqv?
    (test 0.0 (fpulp-error -inf.0 -inf.0))
    (test 0.0 (fpulp-error -max.0 -max.0))
    (test 0.0 (fpulp-error -1.0 -1.0))
    (test 0.0 (fpulp-error -min.0 -min.0))
    (test 0.0 (fpulp-error -0.0 -0.0))
    (test 0.0 (fpulp-error +0.0 +0.0))
    (test 0.0 (fpulp-error +min.0 +min.0))
    (test 0.0 (fpulp-error +1.0 +1.0))
    (test 0.0 (fpulp-error +max.0 +max.0))
    (test 0.0 (fpulp-error +inf.0 +inf.0))

    ;; Both arguments zero
    (test 0.0 (fpulp-error -0.0 +0.0))
    (test 0.0 (fpulp-error +0.0 -0.0))

    ;; LHS argument +inf.0
    (test +inf.0 (fpulp-error +inf.0 -inf.0))
    (test +inf.0 (fpulp-error +inf.0 -max.0))
    (test +inf.0 (fpulp-error +inf.0 -1.0))
    (test +inf.0 (fpulp-error +inf.0 -min.0))
    (test +inf.0 (fpulp-error +inf.0 -0.0))
    (test +inf.0 (fpulp-error +inf.0 +0.0))
    (test +inf.0 (fpulp-error +inf.0 +min.0))
    (test +inf.0 (fpulp-error +inf.0 +1.0))
    (test +inf.0 (fpulp-error +inf.0 +max.0))

    ;; LHS argument -inf.0
    (test +inf.0 (fpulp-error -inf.0 -max.0))
    (test +inf.0 (fpulp-error -inf.0 -1.0))
    (test +inf.0 (fpulp-error -inf.0 -min.0))
    (test +inf.0 (fpulp-error -inf.0 -0.0))
    (test +inf.0 (fpulp-error -inf.0 +0.0))
    (test +inf.0 (fpulp-error -inf.0 +min.0))
    (test +inf.0 (fpulp-error -inf.0 +1.0))
    (test +inf.0 (fpulp-error -inf.0 +max.0))
    (test +inf.0 (fpulp-error -inf.0 +inf.0))

    ;; RHS argument +inf.0
    (test +inf.0 (fpulp-error -max.0 +inf.0))
    (test +inf.0 (fpulp-error -1.0 +inf.0))
    (test +inf.0 (fpulp-error -min.0 +inf.0))
    (test +inf.0 (fpulp-error -0.0 +inf.0))
    (test +inf.0 (fpulp-error +0.0 +inf.0))
    (test +inf.0 (fpulp-error +min.0 +inf.0))
    (test +inf.0 (fpulp-error +1.0 +inf.0))
    (test +inf.0 (fpulp-error +max.0 +inf.0))

    ;; RHS argument -inf.0
    (test +inf.0 (fpulp-error -max.0 -inf.0))
    (test +inf.0 (fpulp-error -1.0 -inf.0))
    (test +inf.0 (fpulp-error -min.0 -inf.0))
    (test +inf.0 (fpulp-error -0.0 -inf.0))
    (test +inf.0 (fpulp-error +0.0 -inf.0))
    (test +inf.0 (fpulp-error +min.0 -inf.0))
    (test +inf.0 (fpulp-error +1.0 -inf.0))
    (test +inf.0 (fpulp-error +max.0 -inf.0))

    ;; RHS argument 0.0
    (test +inf.0 (fpulp-error -max.0 +0.0))
    (test +inf.0 (fpulp-error -1.0 +0.0))
    (test +inf.0 (fpulp-error -min.0 +0.0))
    (test +inf.0 (fpulp-error +min.0 +0.0))
    (test +inf.0 (fpulp-error +1.0 +0.0))
    (test +inf.0 (fpulp-error +max.0 +0.0))

    ;; RHS argument -0.0
    (test +inf.0 (fpulp-error -max.0 -0.0))
    (test +inf.0 (fpulp-error -1.0 -0.0))
    (test +inf.0 (fpulp-error -min.0 -0.0))
    (test +inf.0 (fpulp-error +min.0 -0.0))
    (test +inf.0 (fpulp-error +1.0 -0.0))
    (test +inf.0 (fpulp-error +max.0 -0.0))

    ;; Small errors
    (test 1.0 (fpulp-error +0.0 -min.0))
    (test 1.0 (fpulp-error +0.0 +min.0))
    (test 1.0 (fpulp-error -0.0 -min.0))
    (test 1.0 (fpulp-error -0.0 +min.0))
    (test 2.0 (fpulp-error -min.0 +min.0))
    (test 2.0 (fpulp-error +min.0 -min.0))

    (define large-fpulp-error-xys
      (list (list -1.0 -max.0)
            (list -1.0 +max.0)
            (list -min.0 -max.0)
            (list -min.0 +max.0)
            (list -1.0 +1.0)
            (list -max.0 +max.0)
            (list -max.0 -1.0)
            (list -max.0 -min.0)
            (list -max.0 +min.0)
            (list -max.0 +1.0)
            (list -1.0 -min.0)
            (list -1.0 +min.0)
            (list -min.0 -1.0)
            (list -min.0 +1.0)
            (list -0.0 -max.0)
            (list -0.0 -1.0)
            (list -0.0 +1.0)
            (list -0.0 +max.0)
            (list +0.0 -max.0)
            (list +0.0 -1.0)
            (list +0.0 +1.0)
            (list +0.0 +max.0)
            (list +min.0 -max.0)
            (list +min.0 -1.0)
            (list +min.0 +1.0)
            (list +min.0 +max.0)
            (list +1.0 -max.0)
            (list +1.0 -1.0)
            (list +1.0 -min.0)
            (list +1.0 +min.0)
            (list +1.0 +max.0)
            (list +max.0 -max.0)
            (list +max.0 -1.0)
            (list +max.0 -min.0)
            (list +max.0 +min.0)
            (list +max.0 +1.0)))

    ;; Large errors
    (let loop ([xys large-fpulp-error-xys])
      ;; (match-define (list x y) xy)
      (unless (null? xys)
        (let ((xy (car xys)))
          (let ((x (car xy))
                (y (cadr xy)))
            (test-assert (>= (fpulp-error x y) (expt 2 52))
              ;; (format "x = ~a  y = ~a" x y)
              )))
        (loop (cdr xys))))

    (test 1.0 (fpulp-error 1.0 (fpnext 1.0)))
    (test 1.0 (fpulp-error +max.0 (fpprev +max.0)))

    (repeat 1000
      (let* ([s (random)]
             [e (fp (random-integer -1074 1024))]
             [x (* s (fpexpt 2.0 e))])
        (test 1.0 (fpulp-error x (fpnext x))
              ;; (format "x = ~a" x)
              ))))

  ;; ===================================================================================================
  ;; fllog2

  ;; Make sure it's exact for exact powers of two
  ;; for ([x  (in-range -1074.0 1024.0)])
  (test-group "fplog2"
    (let loop ([x -1074.0])
      (unless (= x 1024.0)
        (let* ((y (fpexp2 x))
               (x0 (fplog2 y)))
          (test x0 x)))))

  ;; ===================================================================================================
  ;; fl2 tests

  (test-group "fp2"
    ;; for* ([x2  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)]
    ;;        [x1  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)])
    (let loop ([nums (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)])
      (unless (null? nums)
        (let* ((x1 (car nums))
               (x2 (car nums))
               (n (count (lambda (b) b)
                         (map (lambda (f) (f x2 x1))
                              (list fp2rational? fp2infinite? fp2nan?)))))
          (test-assert (= n 1))
          ;; (unless (= n 1) (printf "x2 = ~v  x1 = ~v~n" x2 x1))
          )
        (loop (cdr nums))))

    #|
    Tests to add

    (for*: ([x2  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)]
    [x1  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)])
    (define n
    (count (λ: ([b : Boolean]) b)
    (map (λ: ([f : (Flonum Flonum -> Boolean)])
    (f x2 x1))
    (list fl2rational? fl2infinite? fl2nan?))))
    (unless (= n 1) (printf "x2 = ~v  x1 = ~v~n" x2 x1)))

    fl2=
    fl2>
    fl2<
    fl2>=
    fl2<=

    (fl2step x2 x1 n/2) twice = (fl2step x2 x1 n)
    |#

    (test-assert (let-values ([(y2 y1)  (fp+/error +max.hi +max.lo)])
                   (fp2= y2 y1 +max.hi +max.lo)))

    (test-assert (let*-values ([(y2 y1)  (fp2next +max.hi +max.lo)])
                   (fp2infinite? y2 y1))))

  ;; ===================================================================================================
  ;; FPU testing

  ;; (test '()
  ;;       (parameterize ([print-fp-test-progress? #f])
  ;;         (test-floating-point 10000)))

  ;; (test-group "Extra tests"
  ;;   (import (math flonum tests))
  ;; ;; TODO: figure out how even to understand the extra tests
  ;;   )

  )
