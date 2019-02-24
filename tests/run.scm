(import test)

(current-test-verbosity #f)

(test-begin "math")

(include-relative "../utils.scm")
(include-relative "number-theory.scm")

(test-end "math")
