(test-group "math.flonum"

  (import math.flonum
          math.base
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
                    )))))
