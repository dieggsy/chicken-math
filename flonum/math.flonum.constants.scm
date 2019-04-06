(module math.flonum.constants (-max.0
                               -max-subnormal.0
                               -min.0
                               +min.0
                               +max-subnormal.0
                               +max.0
                               epsilon.0)
  (import scheme
          (only chicken.flonum maximum-flonum)
          (only math.flonum.bits
                ordinal->flonum
                fpnext
                fpprev
                fpulp)
          (only miscmacros ensure))


  (define -max.0 (ensure negative? (fpnext -inf.0)))
  (define -min.0 (ensure negative? (fpprev 0.0)))
  (define +min.0 (ensure positive? (fpnext 0.0)))
  (define +max.0 (ensure positive? (fpprev +inf.0)))

  (define +max-subnormal.0 (ensure positive? (ordinal->flonum #xfffffffffffff)))
  (define -max-subnormal.0 (- +max-subnormal.0))

  ;; The smallest flonum that can be added to 1.0 to get a result != 1.0
  (define epsilon.0 (ensure positive? (fpulp 1.0))))
