(module math.base.functions (power-of-two?
                             absolute-error
                             relative-error
                             sum
                             asinh acosh atanh
                             float-complex?
                             ;; inline-number->float-complex
                             number->float-complex)

  (import scheme
          chicken.type
          chicken.base
          (only chicken.bitwise integer-length)
          math.flonum.functions
          math.flonum.more-functions
          math.flonum.fpvector
          math.racket-shim)

  ;; Returns #t if x is an integer power of 2
  (: power-of-two? (number -> boolean))
  (define (power-of-two? x)
    (cond [(not (positive? x))  #f]
          [(flonum? x)  (fp= x (fpexpt 2.0 (fpround (fp/ (fplog x) (fplog 2.0)))))]
          ;; [(single-flonum? x)  (power-of-two? (fp x))]
          [(integer? x)  (= x (expt 2 (- (integer-length x) 1)))]
          [else  (and (= 1 (numerator x))
                      (power-of-two? (denominator x)))]))

  (: fix-exact-return (number number number -> number))
  (define (fix-exact-return x r e)
    (cond ;; [(or (single-flonum? x) (single-flonum? r))  (real->single-flonum e)]
     [(or (flonum? x) (flonum? r))  (fp e)]
     [else  e]))

  (: absolute-error (number number -> number))
  (define (absolute-error x r)
    (fix-exact-return
     x r (cond [(eqv? x r)  0]
               [(and (rational? x) (rational? r))
                (abs (- (inexact->exact x) (inexact->exact r)))]
               [else  +inf.0])))

  (: relative-error (number number -> number))
  (define (relative-error x r)
    (fix-exact-return
     x r (cond [(eqv? x r)  0]
               [(and (zero? x) (zero? r))  0]
               [(zero? r)  +inf.0]
               [(and (rational? x) (rational? r))
                (let ((exact-r (inexact->exact r)))
                  (abs (/ (- (inexact->exact x) exact-r) exact-r)))]
               [else  +inf.0])))

  (: sum ((list-of number) -> number))
  (define (sum xs)
    (let loop ([xs xs]
               [r  0]
               [fs  '()])
      (cond [(null? xs)
             (cond [(null? fs)  r]
                   [(zero? r)  (fpsum fs)]
                   [else  (fp (+ r (inexact->exact (fpsum fs))))])]
            [else
             (let ([x  (car xs)]
                   [xs  (cdr xs)])
               (cond [(flonum? x)  (loop xs r (cons x fs))]
                     ;; [(single-flonum? x)  (loop xs r (cons (fl x) fs))]
                     [else  (loop xs (+ x r) fs)]))])))

  ;; ===================================================================================================
  ;; Inverse hyperbolic

  (: asinh (number -> number))
  (define (asinh x)
    (cond [(flonum? x)  (fpasinh x)]
          [(eqv? x 0)  0]
          [(real? x)  (fpasinh (fp x))]
          [(float-complex? x)  (log (+ x (sqrt (+ (* x x) 1.0))))]
          [else  (log (+ x (sqrt (+ (* x x) 1))))]))

  (: acosh (number -> number))
  (define (acosh x)
    (cond [(flonum? x)  (fpacosh x)]
          [(eqv? x 1)  0]
          [(and (real? x) (>= x 1))  (fpacosh (fp x))]
          [(float-complex? x)  (log (+ x (* (sqrt (+ x 1.0)) (sqrt (- x 1.0)))))]
          [else  (log (+ x (* (sqrt (+ x 1)) (sqrt (- x 1)))))]))

  (: atanh (number -> number))
  (define (atanh x)
    (cond [(flonum? x)  (fpatanh x)]
          [(eqv? x 0)  0]
          [(real? x)  (fpatanh (fp x))]
          [(float-complex? x)  (* 0.5 (- (log (+ 1.0 x)) (log (- 1.0 x))))]
          [else  (* 1/2 (- (log (+ 1 x)) (log (- 1 x))))]))

  ;; ===================================================================================================
  ;; Float-Complex functions

  (define (float-complex? a)
    (and (complex? a)
         (flonum? (real-part a))
         (flonum? (imag-part a))))
  ;; (define-predicate float-complex? Float-Complex)

  ;; (module syntax-defs racket/base
  ;;   (require (for-syntax racket/base
  ;;                        typed/untyped-utils)
  ;;            (only-in typed/racket/base : number let:)
  ;;            racket/flonum)

  ;;   (provide inline-number->float-complex)

  ;;   (define-syntax (inline-number->float-complex stx)
  ;;     (syntax-case stx ()
  ;;       [(_ z-expr)
  ;;        (syntax/loc stx
  ;;          (let: ([z : number  z-expr])
  ;;            (if (number? z)
  ;;                (make-rectangular (real->double-flonum (real-part z))
  ;;                                  (real->double-flonum (imag-part z)))
  ;;                (raise-argument-error 'number->float-complex "number?" z))))]))

  ;;   )
                                        ; module

  ;; (require 'syntax-defs)

  (: number->float-complex (number -> number))
  (define (number->float-complex z)
    (make-rectangular (fp (real-part z))
                      (fp (imag-part z)))))

