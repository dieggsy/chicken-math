(import test)

(current-test-verbosity #f)

(test-begin "math")

(import math.racket-shim)

(include-relative "flonum.scm")
(include-relative "number-theory.scm")
;; (include-relative "bigfloat.scm")

(test-end "math")
(test-exit)
