(module math.private.number-theory (solve-chinese
                                    odd-prime?
                                    prime?)

  (import scheme
          chicken.base
          chicken.bitwise
          chicken.format
          math.private.small-primes
          math.private.divisibility
          math.private.modular-arithmetic
          math.private.base-random
          (only miscmacros ensure)
          (only iset
                make-bit-vector
                bit-vector-set!
                bit-vector-ref))
  (define (andmap fn ls0)
    (let mapf ((ls ls0))
      (or
       (null? ls)
       (and
        (fn (car ls))
        (mapf (cdr ls))))))

  ;;
  ;; Configuration
  ;;

  (define prime-strong-pseudo-certainty 1/10000000)
  (define prime-strong-pseudo-trials
    (integer-length (ensure integer? (/ 1 prime-strong-pseudo-certainty))))

  (define *VERY-SMALL-PRIME-LIMIT* 1000)
  ;; Determines the size of the pre-built table of very small primes
  (define *SMALL-FACTORIZATION-LIMIT* *VERY-SMALL-PRIME-LIMIT*)
  ;; Determines whether to use naive factorization or Pollards rho method.

  ;; ;;
  ;; ;; Powers
  ;; ;;

  ;; ;; (max-dividing-power p n) = m  <=> p^m | n  and  p^(m+1) doesn't divide n
  ;; ;;   In Mathematica this one is called IntegerExponent
  ;; (define (max-dividing-power p n)
  ;;   (define (find-start p-to-e e)
  ;;     ;;(display (list 'fs 'p-to-e p-to-e  'e e)) (newline)
  ;;     ;; p-to-e divides n  and  p-to-e = p^e
  ;;     (let ([p-to-e2 (sqr p-to-e)])
  ;;       (cond [(= p-to-e2 n) (* 2 e)]
  ;;             [(> p-to-e2 n) (find-power p-to-e e)]
  ;;             [(divides? p-to-e2 n) (if (divides? p (quotient n p-to-e2))
  ;;                                       (find-start p-to-e2 (* 2 e))
  ;;                                       (* 2 e))]
  ;;             [else (find-power p-to-e e)])))
  ;;   (define (find-power p-to-e e)
  ;;     ;;(display (list 'fp 'p-to-e p-to-e  'e e)) (newline)
  ;;     ;; p-to-e <= n < (square p-to-e)
  ;;     (+ e (max-dividing-power-naive p (quotient n p-to-e))))
  ;;   (cond [(= p 1) 1]
  ;;         [(not (divides? p n)) 0]
  ;;         [else (ensure (conjoin positive? integer?)
  ;;                       (find-start p 1))]))

  ;; (define (max-dividing-power-naive p n)
  ;;   ;; sames as max-dividing-power but using naive algorithm
  ;;   (define (loop p-to-e e)
  ;;     (if (divides? p-to-e n)
  ;;         (loop (* p p-to-e) (+ e 1))
  ;;         (- e 1)))
  ;;   (if (= p 1)
  ;;       (error 'max-dividing-power "No maximal power of 1 exists")
  ;;       (ensure (conjoin positive? integer?)
  ;;               (loop 1 0))))

  ;; ;; THEOREM (The Chinese Remainder Theorem)
  ;; ;;   Let n1,...,nk be positive integers with gcd(ni,nj)=1 whenever i<>j,
  ;; ;;   and let a1,...,ak be any integers. Then the solutions to
  ;; ;;     x=a1  mod n1,  ...,  x=ak  mod nk
  ;; ;;   has a single solution in {0,...,n-1}, where n=n1*...nk.

  ;; ;; Example : (solve-chinese '(2 3 2) '(3 5 7)) = 23
  ;; (define (solve-chinese as ns)
  ;;   (unless (andmap positive? ns)
  ;;     (raise-argument-error 'solve-chinese "(Listof Positive-Integer)" 1 as ns))
  ;;   ;; the ns should be coprime
  ;;   (let* ([n  (apply * ns)]
  ;;          [cs (map (λ: ([ni : Integer]) (quotient n ni)) ns)]
  ;;          [ds (map modular-inverse cs ns)]
  ;;          [es (cast ds (make-predicate (Listof Integer)))])
  ;;     (cast (modulo (apply + (map * as cs es)) n) natural?)))

  ;;
  ;; PRIMES
  ;;
  (define (odd-prime? n)
    (and (odd? n) (prime? n)))

  ;; PRIMALITY TESTS

  ;; Strong pseudoprimality test
  ;; The strong test returns one of:
  ;;   'probably-prime                                        if n is a prime
  ;;   'composite            (with at least probability 1/2)  if n is a composite non-Carmichael number
  ;;   a proper divisor of n (with at least probability 1/2)  if n is a Carmichael number
  ;; [MCA, p.509 - Algorithm 18.5]
  (define (prime-strong-pseudo-single? n)
    (cond
     [(<= n 0) (error 'prime-strong-pseudo-single? (format "bad argument type - not a positive integer: ~A" n))]
     [(>= n 4)
      (let* ((a (random-integer 2 (- n 1)))
             (g (gcd a n)))
        (cond
         [(> g 1) g] ; factor found
         [else
          ;; 3. write n-1 = 2^ν * m , m odd
          (let loop ([ν 0] [m (- n 1)])
            (cond
             [(even? m) (loop (add1 ν) (quotient m 2))]
             [else ; 4. for i=1,...,ν do bi <- b_{i-1}^2 rem N
              (let ((b (modular-expt a m n)))
                (cond
                 [(= b 1) 'probably-prime]
                 [else
                  (let loop ([i 0] [b b] [b-old b])
                    (if (and (< i ν) (not (= b 1)))
                        (loop (add1 i)
                              (modulo (* b b) n)
                              b)
                        (if (= b 1)
                            (let ([g (gcd (+ b-old 1) n)])
                              (if (or (= g 1) (= g n))
                                  'probably-prime
                                  g))
                            'composite)))]))]))]))]
     [(= n 1)  'composite]
     [else  'probably-prime]))

  (define (prime-strong-pseudo/explanation n)
    ;; run the strong test several times to improve probability
    (define (loop trials result)
      (cond [(= trials 0)                 'very-probably-prime]
            [(eq? result 'probably-prime) (loop (sub1 trials) (prime-strong-pseudo-single? n))]
            [else                         result]))
    (loop prime-strong-pseudo-trials (prime-strong-pseudo-single? n)))

  (define (prime-strong-pseudo? n)
    (let ([explanation (prime-strong-pseudo/explanation n)])
      (or (eq? explanation 'very-probably-prime)
          (eq? explanation #t))))

  (define prime?
    (let ()
      (define N *VERY-SMALL-PRIME-LIMIT*)
      (define ps (make-bit-vector (+ N 1) #t))
      (define ! bit-vector-set!)
      (! ps 0 #f)
      (! ps 1 #f)
      (do ((n 2 (add1 n)))
          ((> n N))
        (do ((m (+ n n) (+ n m)))
            ((> m N))
          (! ps m #f)))
      (lambda (n)
        (let ((n (abs n)))
          (cond ((<= n N)
                 (bit-vector-ref ps n))
                ((< n *SMALL-PRIME-LIMIT*)
                 (small-prime? n))
                (else
                 (prime-strong-pseudo? n)))))))

  )
