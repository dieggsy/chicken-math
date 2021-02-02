(module math.flonum.utils
    ((near-pow2 fpexpt fpmax fpmin fpround fp/ fplog fpabs)
     (near-pow2/div fpmax fpmin fp/ fplog fpabs fpexpt fpround fp* fp+)
     check-fpvector-lengths!)
  (import scheme
          (only miscmacros define-syntax-rule)
          chicken.flonum
          chicken.type
          (only srfi-4 f64vector-length)
          (only chicken.format format))

  (define-syntax-rule (near-pow2 a)
    (fpexpt 2.0 (fpmax -1023.0 (fpmin 1023.0 (fpround (fp/ (fplog (fpabs a)) (fplog 2.0)))))))

  (define-syntax-rule (near-pow2/div a b)
    ;; Clamping both values makes this work properly when a or b is infinite or zero
    (let ([ea  (fpmax -511.0 (fpmin 511.0 (fp/ (fplog (fpabs a)) (fplog 2.0))))]
          [eb  (fpmax -511.0 (fpmin 511.0 (fp/ (fplog (fpabs b)) (fplog 2.0))))])
      (fpexpt 2.0 (fpround (fp* 0.5 (fp+ ea eb))))))

  (: check-fpvector-lengths! (symbol fixnum f64vector #!rest (list-of f64vector) -> void))
  (define (check-fpvector-lengths! name n . xss)
    (import srfi-4
            (only chicken.base void error))
    (let loop ((xss xss))
      (cond ((null? xss)
             (void))
            ((not (fp= (exact->inexact n) (exact->inexact (f64vector-length (car xss)))))
             (error name (format "arg is not an f64vector of length ~a" n)
                    (car xss)))
            (else (loop (cdr xss)))))))
