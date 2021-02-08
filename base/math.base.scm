(module math.base ()
  (import chicken.module)

  (import math.base.random
          math.base.constants
          math.base.functions)
  (reexport math.base.random
            math.base.constants
            math.base.functions))
