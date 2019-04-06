(module math.number-theory ()

  (import chicken.module)

  (import math.number-theory.divisibility
          math.number-theory.modular-arithmetic
          math.number-theory.base
          math.number-theory.factorial
          math.number-theory.binomial
          math.number-theory.bernoulli
          math.number-theory.eulerian-number
          math.number-theory.farey
          math.number-theory.fibonacci
          math.number-theory.partitions
          math.number-theory.polygonal
          math.number-theory.primitive-roots
          math.number-theory.quadratic
          math.number-theory.quadratic-residues
          math.number-theory.tangent-number)

  (reexport math.number-theory.divisibility
            math.number-theory.modular-arithmetic
            math.number-theory.base
            math.number-theory.factorial
            math.number-theory.binomial
            math.number-theory.bernoulli
            math.number-theory.eulerian-number
            math.number-theory.farey
            math.number-theory.fibonacci
            math.number-theory.partitions
            math.number-theory.polygonal
            math.number-theory.primitive-roots
            math.number-theory.quadratic
            math.number-theory.quadratic-residues
            math.number-theory.tangent-number))
