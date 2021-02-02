(import test)

(current-test-verbosity #f)

(test-begin "math")

(import math.racket-shim)
(include-relative "number-theory.scm")

(test-end "math")
(test-exit)
