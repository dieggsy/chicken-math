(module math.number-theory ()

  (import chicken.module)

  (import math.private.divisibility
          math.private.modular-arithmetic
          math.private.number-theory
          math.private.factorial
          math.private.binomial
          math.private.bernoulli
          math.private.eulerian-number
          math.private.farey
          math.private.fibonacci
          math.private.partitions
          math.private.polygonal
          math.private.primitive-roots
          math.private.quadratic
          math.private.quadratic-residues
          math.private.tangent-number)

  (reexport math.private.divisibility
            math.private.modular-arithmetic
            math.private.number-theory
            math.private.factorial
            math.private.binomial
            math.private.bernoulli
            math.private.eulerian-number
            math.private.farey
            math.private.fibonacci
            math.private.partitions
            math.private.polygonal
            math.private.primitive-roots
            math.private.quadratic
            math.private.quadratic-residues
            math.private.tangent-number))
