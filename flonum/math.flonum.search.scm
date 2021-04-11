(module math.flonum.search (find-least-flonum
                            fpfind-least-integer)
  (import scheme
          chicken.type
          (only chicken.base case-lambda when error)
          math.flonum.constants
          math.flonum.functions
          math.flonum.bits)

  (define +inf-ordinal (flonum->ordinal +inf.0))

  (: find-least-flonum ((float -> any) float #!optional float -> (or float false)))
  (define find-least-flonum
    (case-lambda
      [(pred? x-start)
       (when (eqv? +nan.0 x-start)
         (error 'find-least-flonum "not a non-NaN flonum" x-start))
       (let loop ([n-end  (flonum->ordinal x-start)] [step 1])
         (define x-end (ordinal->flonum n-end))
         (cond [(pred? x-end)  (find-least-flonum pred? x-start x-end)]
               [(fp= x-end +inf.0)  #f]
               [else  (loop (min +inf-ordinal (+ n-end step)) (* step 2))]))]
      [(pred? x-start x-end)
       (when (eqv? x-start +nan.0)
         (error 'find-least-flonum "not a non-NaN flonum" x-start))
       (when (eqv? x-end +nan.0)
         (error 'find-least-flonum "not a non-NaN flonum" x-end))
       (cond [(pred? x-start)  x-start]
             [(not (pred? x-end))  #f]
             [else
              (let loop ([n-start  (flonum->ordinal x-start)] [n-end  (flonum->ordinal x-end)])
                (cond [(= n-start n-end)
                       (let ((x (ordinal->flonum n-end)))
                         (if (pred? x) x #f))]
                      [else
                       (let ((n-mid (quotient (+ n-start n-end) 2)))
                         (cond [(pred? (ordinal->flonum n-mid))
                                (loop n-start n-mid)]
                               [else
                                (loop (+ n-mid 1) n-end)]))]))])]))

  (: sub-or-prev (float float -> float))
  (define (sub-or-prev k i)
    (define prev-k (fp- k i))
    (if (fp= prev-k k) (fpprev* k) prev-k))

  (: add-or-next (float float -> float))
  (define (add-or-next k i)
    (define next-k (fp+ k i))
    (if (fp= next-k k) (fpnext* k) next-k))

  (: flmidpoint (float float -> float))
  (define (fpmidpoint x y)
    (let ([x  (fpmin x y)]
          [y  (fpmax x y)])
      (cond [(fp= x -inf.0)  (cond [(fp= y +inf.0)  0.0]
                                   [(fp= y -inf.0)  -inf.0]
                                   [else  (+ (* 0.5 -max.0) (* 0.5 y))])]
            [(fp= y +inf.0)  (cond [(fp= x +inf.0)  +inf.0]
                                   [else  (+ (* 0.5 x) (* 0.5 +max.0))])]
            [else  (+ (* 0.5 x) (* 0.5 y))])))

  (: fpfind-least-integer ((float -> any) #!optional float float float -> float))
  ;; Finds the least integer k such that (pred? k) is #t, given optional bounds and an optional
  ;; initial estimate. If the predicate is not monotone in the bounds, the result of this function is
  ;; indeterminate, and depends in an unspecified way on the initial estimate.
  ;; Formally, to get a unique answer, one of the following cases must be true.
  ;;  1. Exists k, forall mn <= i < k, (pred? i) is #f /\ forall k <= j <= mx, (pred? j) is #t
  ;;  2. Forall k, (pred? k) is #f
  ;;  3. Forall k, (pred? k) is #t
  ;; where mn0 <= k <= mx0. For case #1, this function returns k. For case #2, it returns +nan.0. For
  ;; case #3, it returns mn0.
  (define (fpfind-least-integer pred? #!optional [mn0 -inf.0] [mx0 +inf.0] [k0 +nan.0])
    (let ([mn  (fpceiling (fpmin mn0 mx0))]
          [mx  (fpfloor (fpmax mn0 mx0))])
      ;; Make sure the initial estimate is in-bounds
      (define k (cond [(and (>= k0 mn) (<= k0 mx))  (fpfloor k0)]
                      [else  (fpfloor (fpmidpoint mn mx))]))
      (define k? (pred? k))
      ;; Find an integer k-min <= k for which (pred? k-min) is #f; increment exponentially
      (define-values (k-min k-min?)
        (let loop ([k-min k] [k-min? k?] [i 1.0])
          (assume ((k-min float)
                   (i float))
                                        ;(printf "min: ~v~n" k-min)
            (cond [(fp<= k-min mn)  (cond [(fp= k-min mn)  (values k-min k-min?)]
                                          [else  (values mn (pred? mn))])]
                  [k-min?  (let ((prev-k-min (sub-or-prev k-min i)))
                             (loop prev-k-min (pred? prev-k-min) (* 2.0 (- k-min prev-k-min))))]
                  [else  (values k-min #f)]))))
      ;; Find an integer k-max >= k0 for which (pred? k-max) is #t; increment exponentially
      (define-values (k-max k-max?)
        (let loop  ([k-max k] [k-max? k?] [i 1.0])
                                        ;(printf "max: ~v~n" k-max)
          (assume ((k-max float)
                   (i float))
            (cond [(fp>= k-max mx)  (cond [(fp= k-max mx)  (values k-max k-max?)]
                                          [else  (values mx (pred? mx))])]
                  [k-max?  (values k-max #t)]
                  [else   (let ((next-k-max (add-or-next k-max i)))
                            (loop next-k-max (pred? next-k-max) (* 2.0 (- next-k-max k-max))))]))))
      ;; Quickly check cases #2 and #3; if case #1, do a binary search
      (cond [(not k-max?)  +nan.0]
            [k-min?  mn]
            [else
             ;; Loop invariant: (pred? k-max) is #t and (pred? k-min) is #f
             (let loop ([k-min k-min] [k-max k-max])
                                        ;(printf "~v ~v~n" k-min k-max)
               (define k (fpfloor (fpmidpoint k-min k-max)))
               ;; Check whether k-min + 1 = k-max or (flnext k-min) = k-max
               (cond [(or (= k k-min) (= k k-max))  k-max]
                     [(pred? k)  (loop k-min k)]
                     [else  (loop k k-max)]))]))))
