(import (only miscmacros define-syntax-rule)
        chicken.flonum)

(define-syntax-rule (near-pow2 a)
    (fpexpt 2.0 (fpmax -1023.0 (fpmin 1023.0 (fpround (fp/ (fplog (fpabs a)) (fplog 2.0)))))))

  (define-syntax-rule (near-pow2/div a b)
    ;; Clamping both values makes this work properly when a or b is infinite or zero
    (let ([ea  (fpmax -511.0 (fpmin 511.0 (fp/ (fplog (fpabs a)) (fplog 2.0))))]
          [eb  (fpmax -511.0 (fpmin 511.0 (fp/ (fplog (fpabs b)) (fplog 2.0))))])
      (fpexpt 2.0 (fpround (fp* 0.5 (fp+ ea eb))))))
