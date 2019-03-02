(module math.private.flonum-bits (flonum->bit-field
                                  bit-field->flonum
                                  flonum->fields fields->flonum
                                  flonum->sig+exp sig+exp->flonum
                                  flonum->ordinal ordinal->flonum
                                  fpstep fpnext fpprev flonums-between
                                  fpulp)
  (import scheme
          chicken.flonum
          (only chicken.base
                include
                include-relative
                error
                let-values
                case-lambda
                fixnum?
                sub1
                unless)
          (only chicken.format format)
          (only chicken.bitwise)
          (only miscmacros ensure))

  (include "utils.scm")
  (include-relative "racket-shim.scm")

  (define (flonum->bit-field x)
    (ensure natural?
            (integer-bytes->integer
             (real->floating-point-bytes x 8) #f)))

  (define (bit-field->flonum i)
    (cond [(and (>= i 0) (<= i #xffffffffffffffff))
           (floating-point-bytes->real (integer->integer-bytes i 8 #f))]
          [else
           (error 'bit-field->flonum "bad argument type - not an integer in [0 .. #xffffffffffffffff]" i)]))


  (define implicit-leading-one (arithmetic-shift 1 52))
  (define max-significand (- implicit-leading-one 1))
  (define max-exponent 2047)
  (define max-signed-exponent 1023)
  (define min-signed-exponent -1074)

  (define (flonum->fields x)
    (define n (flonum->bit-field x))
    (values (if (zero? (bitwise-bit-field n 63 64)) 0 1)
            ;; TODO: in the original racket code, this check was for index?,
            ;; not fixnum?
            (ensure fixnum? (bitwise-bit-field n 52 63))
            (bitwise-bit-field n 0 52)))


  (define (fields->flonum s e m)
    (cond [(not (or (= s 0) (= s 1)))
           (error 'fields->flonum "bad argument value - not 0 or 1" s)]
          [(or (< e 0) (> e max-exponent))
           (error 'fields->flonum (format "bad argument value - not a natural number <= ~a" max-exponent) e)]
          [(or (< m 0) (> m max-significand))
           (error 'fields->flonum (format "bad argument value - not a natural number <= ~a" max-significand) m)]
          [else
           (bit-field->flonum (bitwise-ior (arithmetic-shift s 63)
                                           (arithmetic-shift e 52)
                                           m))]))

  (define (flonum->sig+exp x)
    (define-values (s e m) (flonum->fields x))
    (let-values ([(sig exp)  (if (= e 0)
                                 (values m -1074)
                                 (values (bitwise-ior m implicit-leading-one)
                                         (ensure fixnum? (- e 1075))))])
      (values (if (zero? s) sig (- sig)) exp)))

  (define (sig+exp->flonum sig exp)
    (cond [(= sig 0)  0.0]
          [(> exp max-signed-exponent)  (if (< sig 0) -inf.0 +inf.0)]
          [(< exp min-signed-exponent)  (if (< sig 0) -0.0 0.0)]
          [else  (exact->inexact (* sig (expt 2 exp)))]))

  (define (flonum->ordinal x)
    (cond [(fp< x 0.0)  (- (flonum->bit-field (fp- 0.0 x)))]
          [else             (flonum->bit-field (fpabs x))])) ; abs for -0.0

  (define (ordinal->flonum i)
    (cond [(and (<= i #x7fffffffffffffff))
           (cond [(< i 0)  (fp- 0.0 (bit-field->flonum (- i)))]
                 [else         (bit-field->flonum i)])]
          [else
           (error 'bit-field->flonum "bad argument type - not an integer in [0 .. #xffffffffffffffff]" i)]))

  (define +inf-ordinal (flonum->ordinal +inf.0))
  (define -inf-ordinal (flonum->ordinal -inf.0))

  (define (fpstep x n)
    (cond [(not (and (fp>= x -inf.0) (fp<= x +inf.0)))  +nan.0]
          [(and (fp= x +inf.0) (>= n 0))  +inf.0]
          [(and (fp= x -inf.0) (<= n 0))  -inf.0]
          [else  (let ((i (+ n (flonum->ordinal x))))
                   (cond [(< i -inf-ordinal)  -inf.0]
                         [(> i +inf-ordinal)  +inf.0]
                         [else  (ordinal->flonum i)]))]))

  (define (fpnext x) (fpstep x 1))

  (define (fpprev x) (fpstep x -1))

  (define (flonums-between x y)
    (- (flonum->ordinal y) (flonum->ordinal x)))

  (define (fpulp x)
    (let ([x  (fpabs x)])
      (cond [(fp= x +inf.0)  +nan.0]
            [(eqv? x +nan.0)  +nan.0]
            [(fp= x 0.0)  0.0]
            [else
             (let ((ulp (fpabs (fp- (fpnext x) x))))
               (cond [(fp= ulp +inf.0)  (fpabs (fp- x (fpprev x)))]
                     [else  ulp]))]))))
