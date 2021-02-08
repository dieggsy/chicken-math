(module math.flonum ()
  (import scheme
          chicken.module)
  (import math.flonum.bits
          math.flonum.constants
          math.flonum.functions
          math.flonum.search
          math.flonum.exp
          math.flonum.log
          math.flonum.more-functions
          math.flonum.factorial
          math.flonum.log1pmx
          math.flonum.error
          math.flonum.brent-dekker
          math.flonum.expansion.base
          math.flonum.expansion.exp
          math.flonum.expansion.log
          math.flonum.fpvector)

  (reexport math.flonum.bits
            math.flonum.constants
            math.flonum.functions
            math.flonum.search
            math.flonum.exp
            math.flonum.log
            math.flonum.more-functions
            math.flonum.factorial
            math.flonum.log1pmx
            math.flonum.error
            math.flonum.brent-dekker
            math.flonum.expansion.base
            math.flonum.expansion.exp
            math.flonum.expansion.log
            math.flonum.fpvector)

  (define lg* fp+)
  (define lp/ fp-)
  (define lgprod fpsum))
