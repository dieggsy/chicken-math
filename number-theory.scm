;; (include "private/number-theory/number")
(module math.number-theory ()

  (import scheme
          chicken.base
          chicken.module)

  (reexport math.private.divisibility)
  (reexport math.private.modular-arithmetic)
  (reexport math.private.number-theory)
  (reexport math.private.factorial))
