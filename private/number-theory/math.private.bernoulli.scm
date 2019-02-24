(module math.private.bernoulli (bernoulli-number)
  (import scheme
          chicken.base
          math.private.binomial
          math.private.vector
          miscmacros)

  (include-relative "../../utils.scm")

  ;; Number of globally memoized Bernoulli numbers
  (define num-global-bs 200)
  ;; Globally memoized numbers
  (define global-bs (make-vector num-global-bs 0))
  (vector-set! global-bs 0 1)
  (vector-set! global-bs 1 -1/2)

  ;;   compute the n'th Bernoulli number
  ;;   <http://mathworld.wolfram.com/BernoulliNumber.html>
  ;;   <http://en.wikipedia.org/wiki/Bernoulli_number>
  (define (bernoulli* n)
    ;; Implementation note:
    ;;   - uses Ramanujan's improvement of the standard recurrence relation
    ;;     of the Bernoulli numbers:
    ;;     <http://en.wikipedia.org/wiki/Bernoulli_number#Ramanujan.27s_congruences>
    ;;   - memoizes previous computations
    ;;   - avoids an explicit call to compute the binomials
    (define local-bs (make-vector (max 0 (- (+ n 1) num-global-bs)) 0))

    (define (bs-ref! n thnk)
      (cond [(< n num-global-bs)
             (vector-ref! global-bs n thnk (conjoin exact? zero?))]
            [else
             (vector-ref! local-bs (- n num-global-bs) thnk (conjoin exact? zero?))]))

    (define (next-binom old x k)
      ;; calculate binom(x,k-6) from the old binom(x,k)
      (let ([k-1 (- k 1)] [k-2 (- k 2)] [k-3 (- k 3)] [k-4 (- k 4)] [k-5 (- k 5)])
        (ensure
         integer?
         (* old
            (/ (* k k-1 k-2 k-3 k-4 k-5 )
               (* (- x k-1) (- x k-2) (- x k-3) (- x k-4) (- x k-5) (- x (- k 6))))))))

    (define (A m M)
      (cond
       [(< M 1) 0]
       [else
        (let ((m-6 (ensure natural? (- m 6))))
          (let-values (((sum bin)
                        (let loop ((sum 0)
                                   (bin (binomial (+ m 3) m-6))
                                   (j 1))
                          (if (> j M)
                              (values sum bin)
                              (loop (+ sum (* bin (bern (ensure natural?
                                                                (- m (* 6 j))))))
                                    (next-binom bin (+ m 3) (- m (* 6 j)))
                                    (add1 j))))))
            sum))]))

    (define (bern n)
      (bs-ref!
       n (lambda ()
           (cond
            [(odd? n)  0]
            [else
             (let ((r (remainder n 6)))
               (cond
                [(= r 0) (/  (- (/ (+ n 3)  3) (A n (quotient    n    6)))  (binomial (+ n 3) n))]
                [(= r 2) (/  (- (/ (+ n 3)  3) (A n (quotient (- n 2) 6)))  (binomial (+ n 3) n))]
                [(= r 4) (/  (- (/ (+ n 3) -6) (A n (quotient (- n 4) 6)))  (binomial (+ n 3) n))]
                ;; n is even, so r can only be 0, 2 or 4
                [else  (error 'unreachable-code)]))]))))
    (bern n))

  (define (bernoulli-number n)
    (cond [(< n 0)
           (error 'bernoulli-number "bad argument type - not a nonnegative integer" n)]
          [else  (bernoulli* n)])))
