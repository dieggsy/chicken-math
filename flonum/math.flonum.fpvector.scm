(module math.flonum.fpvector (inline-build-fpvector
                              build-fpvector
                              (inline-fpvector-map check-fpvector-lengths!)
                              fpvector-map
                              ;; Construction
                              unsafe-fpvector-copy!
                              fpvector-copy!
                              ;; Conversion
                              list->fpvector
                              fpvector->list
                              vector->fpvector
                              fpvector->vector
                              ;; Pointwise operations
                              fpvector-scale
                              fpvector-sqr
                              fpvector-sqrt
                              fpvector-abs
                              fpvector+
                              fpvector*
                              fpvector-
                              fpvector/
                              fpvector-min
                              fpvector-max
                              ;; Sum
                              fpvector-sum
                              fpvector-sums
                              fpsum)
  (import scheme
          chicken.flonum
          chicken.fixnum
          srfi-4
          chicken.type
          (only chicken.base include-relative
                assert case-lambda error exact-integer?
                unless sub1 void when let*-values)
          (only chicken.format format)
          (only miscmacros define-syntax-rule)
          math.flonum.functions
          math.racket-shim
          math.flonum.utils)

  (define-syntax inline-build-fpvector
    (syntax-rules ()
      [(_ n-expr f-expr)
       (let* ([xs (make-f64vector n-expr)]
              [n (f64vector-length xs)])
         ;; (define-syntax new-f
         ;;   (syntax-rules ()
         ;;     [(_ i)
         ;;      (let ([x  (f-expr i)])
         ;;        (if (flonum? x) x (error 'build-flvector "arg is not a flonum" x)))]))
         (let loop ((i 0))
           (if (fx< i n)
               (begin (f64vector-set! xs i (f-expr i))
                      (loop (fx+ i 1))))
           xs))]))

  ;; (define-syntax fpvector-mapfill!
  ;;   (syntax-rules ()
  ;;     [(_ xs n f-expr)
  ;;      (let loop ((i 0))
  ;;        (if (fx< i n)
  ;;            (begin (f64vector-set! xs i (f-expr (f64vector-ref xs i)))
  ;;                   (loop (fx+ i 1))))
  ;;        xs)]))

  (define-syntax inline-fpvector-map
    (syntax-rules ()
      [(_ fx-expr xs-expr)
       (let ([n (f64vector-length xs-expr)])
         (assert (f64vector? xs-expr))
         (define ys (make-f64vector n))
         (let loop ((i 0))
           (if (fx< i n)
               (begin (f64vector-set! ys i (fx-expr (f64vector-ref xs-expr i)))
                      (loop (fx+ i 1))))
           ys))]
      [(_ f-expr xs-expr xss-expr ...)
       (let ([n (f64vector-length xs-expr)])
         (let-syntax ((assert-vector
                       (syntax-rules ()
                         ((_ v)
                          (assert (f64vector? v)
                                  'fpvector-map
                                  "not an f64vector"
                                  v)))))
           (assert-vector xs-expr)
           (assert-vector xss-expr)
           ...)
         (check-fpvector-lengths! 'fpvector-map n xss-expr ...)
         (define ys (make-f64vector n))
         (let loop ((i 0))
           (if (fx< i n)
               (begin (f64vector-set! ys i (f-expr (f64vector-ref xs-expr i)
                                                   (f64vector-ref xss-expr i)
                                                   ...))
                      (loop (fx+ i 1))))
           ys)
         )]))

  (: build-flvector (fixnum (float -> float) -> f64vector))
  (define (build-fpvector n f)
    (inline-build-fpvector n f))

  (: fpvector-map (procedure f64vector #!optional f64vector #!rest (list-of f64vector) -> f64vector))
  (define fpvector-map
    (case-lambda
      [(f xs)
       (inline-fpvector-map f xs)]
      [(f xs ys)
       (inline-fpvector-map f xs ys)]
      [(f xs ys . yss)
       (define n (f64vector-length xs))
       (apply check-fpvector-lengths! 'fpvector-map n ys yss)
       (inline-build-fpvector
        n
        (lambda (i)
          (apply f
                 (f64vector-ref xs i)
                 (f64vector-ref ys i)
                 (map (lambda (ys) (f64vector-ref ys i)) yss))))]))

  ;; ===================================================================================================
  ;; flvector-copy!
  (: unsafe-fpvector-copy! (f64vector integer f64vector integer integer -> void))
  (define (unsafe-fpvector-copy! dest dest-start src src-start src-end)
    (let loop ([i dest-start] [j src-start])
      (when (fx< j src-end)
        (f64vector-set! dest i (f64vector-ref src j))
        (loop (fx+ i 1) (fx+ j 1)))))

  (: fpvector-copy! (f64vector integer f64vector #!optional integer integer -> void))
  (define fpvector-copy!
    (case-lambda
      [(dest dest-start src)
       (fpvector-copy! dest dest-start src 0 (f64vector-length src))]
      [(dest dest-start src src-start)
       (fpvector-copy! dest dest-start src src-start (f64vector-length src))]
      [(dest dest-start src src-start src-end)
       (define dest-len (f64vector-length dest))
       (define src-len (f64vector-length src))
       (cond [(or (< dest-start 0) (> dest-start dest-len))
              (error 'fpvector-copy! (format "expected index <= ~a" dest-len)
                     dest-start)]
             [(or (< src-start 0) (> src-start src-len))
              (error 'fpvector-copy! (format "expected index <= ~a" src-len) src-start)]
             [(or (< src-end 0) (> src-end src-len))
              (error 'fpvector-copy! (format "expected index <= ~a" src-len) src-end)]
             [(< src-end src-start)
              (error 'fpvector-copy! "ending index is smaller than starting index")]
             [(< (- dest-len dest-start) (- src-end src-start))
              (error 'fpvector-copy! "not enough room in target vector")]
             [else
              (unsafe-fpvector-copy! dest dest-start src src-start src-end)])]))

  ;; ===================================================================================================
  ;; Conversion
  (define list->fpvector list->f64vector)
  (define fpvector->list f64vector->list)

  (: vector->fpvector (vector -> f64vector))
  (define (vector->fpvector vs)
    (let* ((len (vector-length vs))
           (xs (make-f64vector len)))
      (do ((i 0 (fx+ i 1)))
          ((= i len))
        (f64vector-set! xs i (vector-ref vs i)))))

  (: fpvector->vector (f64vector -> vector))
  (define (fpvector->vector xs)
    (let* ((len (f64vector-length xs))
           (vs (make-vector len)))
      (do ((i 0 (fx+ i 1)))
          ((= i len))
        (vector-set! vs i (f64vector-ref xs i)))))

  ;; ===================================================================================================
  ;; Pointwise operations
  (define-syntax-rule (lift1 f)
    (lambda (arr)
      (inline-fpvector-map f arr)))

  (define-syntax-rule (lift2 f)
    (lambda (arr0 arr1)
      (inline-fpvector-map f arr0 arr1)))

  (: fpvector-scale (f64vector float -> f64vector))
  (define (fpvector-scale arr y) (inline-fpvector-map (lambda (x) (fp* x y)) arr))

  (: fpvector-sqr (f64vector -> f64vector))
  (define fpvector-sqr (lift1 (lambda (x) (fp* x x))))

  (: fpvector-sqrt (f64vector -> f64vector))
  (define fpvector-sqrt (lift1 fpsqrt))

  (: fpvector-abs (f64vector -> f64vector))
  (define fpvector-abs (lift1 fpabs))

  (: fpvector+ (f64vector f64vector -> f64vector))
  (define fpvector+ (lift2 fp+))

  (: fpvector* (f64vector f64vector -> f64vector))
  (define fpvector* (lift2 fp*))

  (: fpvector- (f64vector #!optional f64vector -> f64vector))
  (define fpvector-
    (case-lambda
      [(arr0)
       (inline-fpvector-map (lambda (x) (fp- 0.0 x)) arr0)]
      [(arr0 arr1)
       (inline-fpvector-map fp- arr0 arr1)]))

  (: fpvector/ (f64vector #!optional f64vector -> f64vector))
  (define fpvector/
    (case-lambda
      [(arr0)
       (inline-fpvector-map (lambda (x) (fp/ 1.0 x)) arr0)]
      [(arr0 arr1)
       (inline-fpvector-map fp/ arr0 arr1)]))

  (: fpvector-min  (f64vector f64vector -> f64vector))
  (define fpvector-min (lift2 fpmin))

  (: fpvector-max  (f64vector f64vector -> f64vector))
  (define fpvector-max (lift2 fpmax))

  ;; ===================================================================================================
  ;; Summation

  #|
  Algorithm adapted from:

  J R Shewchuk. Adaptive Precision Floating-Point Arithmetic and Fast Geometric Predicates.
  Discrete & Computational Geometry, 1996, vol 18, pp 305--363.
  |#

  (: flvector-sum (f64vector -> float))
  ;; Returns the sum of the elements in xs in a way that incurs rounding error only once
  (define (fpvector-sum xs)
    (define n (f64vector-length xs))
    ;; Vector of remainders
    (define rs (make-f64vector n))
    ;; Loop over `xs'
    (let i-loop ([i 0]
                 ;; p = Number of valid remainders in `rs'
                 [p 0])
      (cond
       [(fx< i n)
        ;; Add x consecutively to each remainder, storing the remainder of *those* additions in `rs'
        (let j-loop ([j 0]
                     ;; q = Number of remainders generated by this j-loop:
                     [q 0]
                     [x  (f64vector-ref xs i)])
          (cond
           [(fx< j p)
            ;; Get the largest of x and r, or x if it's not rational
            (let ((r (f64vector-ref rs j)))
              (let*-values ([(x r)  (if (fp< (fpabs x) (fpabs r)) (values r x) (values x r))]
                            ;; Add with remainder
                            [(z) (fp+ x r)]
                            [(hi lo)
                             (cond [(fprational? z)  (values z (fp- r (fp- z x)))]
                                   [else  (values x r)])])
                (cond [(fp= lo 0.0)
                       ;; No remainder: don't store (makes average case O(n*log(n)))
                       (j-loop (fx+ j 1) q hi)]
                      [else
                       ;; Store the remainder, increment the counter
                       (f64vector-set! rs q lo)
                       (j-loop (fx+ j 1) (fx+ q 1) hi)])))]
           [else
            ;; Store the sum so far as the last remainder
            (f64vector-set! rs q x)
            (i-loop (fx+ i 1) (fx+ q 1))]))]
       [else
        ;; Add all the remainders
        (let j-loop ([j 0] [acc  0.0])
          (cond [(fx< j p)  (j-loop (fx+ j 1) (fp+ acc (f64vector-ref rs j)))]
                [else  acc]))])))

  (: flvector-sums (f64vector -> f64vector))
  ;; Returns the partial sums of the elements in xs in a way that incurs rounding error only once
  ;; for each
  ;; This function works just like `flvector-sum', but keeps track of partial sums instead of
  ;; summing all the remainders at the end
  (define (fpvector-sums xs)
    (define n (f64vector-length xs))
    (define rs (make-f64vector n))
    (define ss (make-f64vector n))
    (let i-loop ([i 0]
                 [p 0])
      (cond
       [(fx< i n)
        (let j-loop ([j 0]
                     [q 0]
                     [x (f64vector-ref xs i)]
                     [s 0.0])
          (cond
           [(fx< j p)
            (let ((r (f64vector-ref rs j)))
              (let*-values ([(x r)  (if (fp< (fpabs x) (fpabs r)) (values r x) (values x r))]
                            [(z) (fp+ x r)]
                            [(hi lo)
                             (cond [(fprational? z)  (values z (fp- r (fp- z x)))]
                                   [else  (values x r)])])
                (cond [(fp= lo 0.0)
                       (j-loop (fx+ j 1) q hi s)]
                      [else
                       (f64vector-set! rs q lo)
                       (j-loop (fx+ j 1) (fx+ q 1) hi (fp+ s lo))])))]
           [else
            (f64vector-set! rs q x)
            (f64vector-set! ss i (fp+ s x))
            (i-loop (fx+ i 1) (fx+ q 1))]))]
       [else  ss])))

  (: flsum ((list-of float) -> float))
  (define (fpsum xs) (fpvector-sum (list->fpvector xs))))
