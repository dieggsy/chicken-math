(module math.number-theory.base (solve-chinese

                                    ;; primes
                                    nth-prime
                                    random-prime
                                    next-prime
                                    next-primes
                                    prev-prime
                                    prev-primes
                                    prime?
                                    odd-prime?
                                    factorize
                                    defactorize
                                    divisors
                                    prime-divisors
                                    prime-exponents
                                    prime-omega

                                    ;; roots
                                    integer-root
                                    integer-root/remainder

                                    ;; Powers
                                    max-dividing-power
                                    perfect-power
                                    perfect-power?
                                    prime-power
                                    prime-power?
                                    odd-prime-power?
                                    as-power
                                    perfect-square

                                    ;; number theoretic functions
                                    totient
                                    moebius-mu
                                    divisor-sum
                                    mangoldt-lambda)

  (import scheme
          chicken.type
          (only chicken.base
                include
                add1
                sub1
                case-lambda
                let-values
                error
                unless
                when)
          (only chicken.bitwise arithmetic-shift integer-length)
          (only chicken.sort sort)
          (only srfi-1 every first second)
          (only math.number-theory.small-primes small-prime? *SMALL-PRIME-LIMIT*)
          (only math.number-theory.divisibility divides?)
          (only math.number-theory.modular-arithmetic modular-expt modular-inverse)
          (only math.base.random random-natural random-integer)
          (only miscmacros ensure))

  (include "utils.scm")

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

  ;;
  ;; Powers
  ;;

  (: max-dividing-power (integer integer -> integer))
  ;; (max-dividing-power p n) = m  <=> p^m | n  and  p^(m+1) doesn't divide n
  ;;   In Mathematica this one is called IntegerExponent
  (define (max-dividing-power p n)
    (define (find-start p-to-e e)
      ;;(display (list 'fs 'p-to-e p-to-e  'e e)) (newline)
      ;; p-to-e divides n  and  p-to-e = p^e
      (let ([p-to-e2 (expt p-to-e 2)])
        (cond [(= p-to-e2 n) (* 2 e)]
              [(> p-to-e2 n) (find-power p-to-e e)]
              [(divides? p-to-e2 n) (if (divides? p (quotient n p-to-e2))
                                        (find-start p-to-e2 (* 2 e))
                                        (* 2 e))]
              [else (find-power p-to-e e)])))
    (define (find-power p-to-e e)
      ;;(display (list 'fp 'p-to-e p-to-e  'e e)) (newline)
      ;; p-to-e <= n < (square p-to-e)
      (+ e (max-dividing-power-naive p (quotient n p-to-e))))
    (cond [(= p 1) 1]
          [(not (divides? p n)) 0]
          [else (ensure natural?
                        (find-start p 1))]))

  (: max-dividing-power-naive (integer integer -> integer))
  (define (max-dividing-power-naive p n)
    ;; sames as max-dividing-power but using naive algorithm
    (define (loop p-to-e e)
      (if (divides? p-to-e n)
          (loop (* p p-to-e) (+ e 1))
          (- e 1)))
    (if (= p 1)
        (error 'max-dividing-power "No maximal power of 1 exists")
        (ensure natural?
                (loop 1 0))))

  ;; THEOREM (The Chinese Remainder Theorem)
  ;;   Let n1,...,nk be positive integers with gcd(ni,nj)=1 whenever i<>j,
  ;;   and let a1,...,ak be any integers. Then the solutions to
  ;;     x=a1  mod n1,  ...,  x=ak  mod nk
  ;;   has a single solution in {0,...,n-1}, where n=n1*...nk.

  ;; Example : (solve-chinese '(2 3 2) '(3 5 7)) = 23
  (: solve-chinese ((list-of integer) (list-of integer) -> integer))
  (define (solve-chinese as ns)
    (unless (andmap positive? ns)
      (error 'solve-chinese "bad argument type - not a list of positive integers" ns))
    ;; the ns should be coprime
    (let* ([n  (apply * ns)]
           [cs (map (lambda (ni) (quotient n ni)) ns)]
           [ds (map modular-inverse cs ns)]
           [es ds])
      (modulo (apply + (map * as cs es)) n)))

  ;;
  ;; PRIMES
  ;;
  (: odd-prime? (integer -> boolean))
  (define (odd-prime? n)
    (and (odd? n) (prime? n)))

  ;; PRIMALITY TESTS

  ;; Strong pseudoprimality test
  ;; The strong test returns one of:
  ;;   'probably-prime                                        if n is a prime
  ;;   'composite            (with at least probability 1/2)  if n is a composite non-Carmichael number
  ;;   a proper divisor of n (with at least probability 1/2)  if n is a Carmichael number
  ;; [MCA, p.509 - Algorithm 18.5]

  (: prime-strong-pseudo-single? (integer -> (or symbol integer)))
  (define (prime-strong-pseudo-single? n)
    (cond
     [(<= n 0) (error 'prime-strong-pseudo-single? "bad argument type - not a positive integer" n)]
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

  (: prime-strong-pseudo/explanation (integer -> (or symbol integer)))
  (define (prime-strong-pseudo/explanation n)
    ;; run the strong test several times to improve probability
    (define (loop trials result)
      (cond [(= trials 0)                 'very-probably-prime]
            [(eq? result 'probably-prime) (loop (sub1 trials) (prime-strong-pseudo-single? n))]
            [else                         result]))
    (loop prime-strong-pseudo-trials (prime-strong-pseudo-single? n)))

  (: prime-strong-pseudo? (integer -> boolean))
  (define (prime-strong-pseudo? n)
    (let ([explanation (prime-strong-pseudo/explanation n)])
      (or (eq? explanation 'very-probably-prime)
          (eq? explanation #t))))


  (: prime? (integer -> boolean))
  (define prime?
    (let ()
      (define N *VERY-SMALL-PRIME-LIMIT*)
      (define ps (make-vector (+ N 1) #t))
      (define ! vector-set!)
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
                 (vector-ref ps n))
                ((< n *SMALL-PRIME-LIMIT*)
                 (small-prime? n))
                (else
                 (prime-strong-pseudo? n)))))))


  (: next-prime (integer -> integer))
  (define (next-prime n)
    (cond
     [(negative? n) (- (prev-prime (abs n)))]
     [(= n 0) 2]
     [(= n 1) 2]
     [(= n 2) 3]
     [(even? n)
      (let ([n+1 (add1 n)])
        (if (prime? n+1)
            n+1
            (next-prime n+1)))]
     [else
      (let ([n+2 (+ n 2)])
        (if (prime? n+2)
            n+2
            (next-prime n+2)))]))

  (: prev-prime (integer -> integer))
  (define (prev-prime n)
    (cond
     [(negative? n) (- (next-prime (abs n)))]
     [(= n 3)   2]
     [(< n 3)   -2]
     [(even? n)
      (let ([n-1 (sub1 n)])
        (if (prime? n-1)
            n-1
            (prev-prime n-1)))]
     [else (let ([n-2 (- n 2)])
             (if (prime? n-2)
                 n-2
                 (prev-prime n-2)))]))

  (: next-primes (integer integer -> (list-of integer)))
  (define (next-primes m primes-wanted)
    (cond
     [(< primes-wanted 0)
      (error 'next-primes "bad argument type - not a nonnegative integer" primes-wanted)]
     [else
      (let loop ((n m)
                 (primes-wanted primes-wanted))
        (if (= primes-wanted 0)
            '()
            (let ([next (next-prime n)])
              (if next
                  (cons next (loop next (sub1 primes-wanted)))
                  '()))))]))

  (: prev-primes (integer integer -> (list-of integer)))
  (define (prev-primes m primes-wanted)
    (cond
     [(< primes-wanted 0) (error 'next-primes "bad argument type - not a nonnegative integer" primes-wanted)]
     [else
      (let loop ((n m)
                 (primes-wanted primes-wanted))
        (if (= primes-wanted 0)
            '()
            (let ([prev (prev-prime n)])
              (if prev
                  (cons prev (loop prev (sub1 primes-wanted)))
                  '()))))]))

  (: nth-prime (integer -> integer))
  (define (nth-prime n)
    (cond [(< n 0) (error 'next-primes "bad argument type - not a nonnegative integer" n)]
          [else
           (let loop ((m 0)
                      (p 2))
             (if (= m n)
                 p
                 (loop (add1 m) (next-prime p))))]))

  (: random-prime (integer -> integer))
  (define (random-prime n)
    (when (<= n 2)
      (error 'random-prime "bad argument type - not a positive integer greater than 2" n))
    (define p (random-natural n))
    (if (prime? p)
        p
        (random-prime n)))

  ;;
  ;; FACTORIZATION
  ;;

  (: factorize (integer -> (list-of (list integer integer))))
  (define (factorize n)
    (if (< n *SMALL-FACTORIZATION-LIMIT*)  ; NOTE: Do measurement of best cut
        (factorize-small n)
        (factorize-large n)))

  (: defactorize ((list-of (list integer integer)) -> integer))
  (define (defactorize bes)
    (cond [(null? bes) 1]
          [else
           (let ((be (first bes)))
             (* (expt (first be) (second be))
                (defactorize (cdr bes))))]))

  (: factorize-small (integer -> (list-of (list integer integer))))
  (define (factorize-small n)
    ;; fast for small n, but works correctly for large n too
    (small-prime-factors-over n 2))

  (: small-prime-factors-over (integer integer -> (list-of (list integer integer))))
  ;; Factor a number n without prime factors below the prime p.
  (define (small-prime-factors-over n p) ; p prime
    (cond [(<= p 0)
           (error 'small-prime-factors-over "bad argument type - not a positive integer" p)]
          [(< n p) '()]
          [(= n p) (list (list p 1))]
          [(prime? n) (list (list n 1))]
          [(divides? p n)
           (let ([m (max-dividing-power p n)])
             (cons (list p m)
                   (small-prime-factors-over
                    (quotient n (expt p m))
                    (next-prime p))))]
          [else (small-prime-factors-over n (next-prime p))]))


  ;;; ALGORITHM 19.8  Pollard's rho method
  ;; INPUT   n>=3 neither a prime nor a perfect power
  ;; OUTPUT  Either a proper divisor of n or #f
  (: pollard (integer -> (or integer false)))
  (define (pollard n)
    (let ([x0 (random-natural n)])
      (do ([xi x0 (remainder (+ (* xi xi) 1) n)]
           [yi x0 (remainder (+ (expt (+ (* yi yi) 1) 2) 1) n)]
           [i  0  (add1 i)]
           [g  1  (gcd (- xi yi) n)])
          [(or (< 1 g n) (> i (sqrt n)))
           (if (< 1 g n)
               g
               #f)])))

  (: pollard-factorize (integer -> (list-of (list integer integer))))
  (define (pollard-factorize n)
    (if (< n *SMALL-FACTORIZATION-LIMIT*)
        (factorize-small n)
        (cond
         [(= n 1)        '()]
         [(prime? n)     `((,n 1))]
         [(even? n)      `((2 1) ,@(pollard-factorize (quotient n 2)))]
         [(divides? 3 n) `((3 1) ,@(pollard-factorize (quotient n 3)))]
         [(simple-perfect-power n)
          => (lambda (base-and-exp)
               (cond
                [(prime? (car base-and-exp)) (list base-and-exp)]
                [else (map (lambda (b-and-e)
                             (list (car b-and-e)
                                   (* (cadr base-and-exp) (cadr b-and-e))))
                           (pollard-factorize (car base-and-exp)))]))]
         [else
          (let loop ([divisor (pollard n)])
            (if divisor
                (append (pollard-factorize divisor)
                        (pollard-factorize (quotient n divisor)))
                (loop (pollard n))))])))

  (: factorize-large (integer -> (list-of (list integer integer))))
  (define (factorize-large n)
    (combine-same-base
     (sort (pollard-factorize n) base-and-exponent<?)))


  (: base-and-exponent<? ((or integer (list integer integer)) (or integer (list integer integer)) -> boolean))
  (define (base-and-exponent<? x y)
    (let ([id-or-first
           (lambda (x)
             (if (number? x) x (first x)))])
      (<= (id-or-first x) (id-or-first y))))

  (: combine-same-base ((list-of (list integer integer)) -> (list-of (list integer integer))))
  (define (combine-same-base list-of-base-and-exponents)
    ;; list-of-base-and-exponents must be sorted
    (let ([l list-of-base-and-exponents])
      (cond
       [(null? l) '()]
       [(null? (cdr l)) l]
       [else
        (let ((b1 (first  (first l)))
              (e1 (second (first l)))
              (b2 (first  (second l)))
              (e2 (second (second l)))
              (more (cddr l)))
          (if (= b1 b2)
              (combine-same-base (cons (list b1 (+ e1 e2))
                                       (cdr (cdr list-of-base-and-exponents))))
              (cons (car list-of-base-and-exponents)
                    (combine-same-base (cdr list-of-base-and-exponents)))))])))

  ;; find-tail pred clist -> pair or false
  ;; Return the first pair of clist whose car satisfies pred. If no pair does, return false.
  ;; (define (find-tail pred xs)
  ;;   (cond [(null? xs) #f]
  ;;         [(pred (car xs)) xs]
  ;;         [else (find-tail pred (cdr xs))]))


  ;;
  ;; Powers
  ;;
  ;;   Write a>0 as b^r with r maximal. Return b and r.
  (: as-power (integer -> integer integer))
  (define (as-power a)
    (let ([r (apply gcd (map second (factorize a)))])
      (values (integer-root a r) r)))

  (: prime-power (integer -> (or (list integer integer) false)))
  ;;   if n is a prime power, return list of prime and exponent in question,
  ;;   otherwise return #f
  (define (prime-power n)
    (let ([factorization (prime-divisors/exponents n)])
      (if (= (length factorization) 1)
          (first (prime-divisors/exponents n))
          #f)))

  (: prime-power? (integer -> boolean))
  ;;   Is n of the form p^m, with p is prime?
  (define (prime-power? n)
    (and (prime-power n) #t))

  (: odd-prime-power? (integer -> boolean))
  (define (odd-prime-power? n)
    (let ([p/e (prime-power n)])
      (and p/e
           (odd? (first p/e)))))

(: perfect-power? (integer -> boolean))
  (define (perfect-power? a)
    (and (not (zero? a))
         (let-values ([(base n) (as-power a)])
           (and (> n 1) (> a 1)))))

  (: simple-perfect-power (integer -> (or (list integer integer) false)))
  (define (simple-perfect-power a)
    ;; simple-perfect-power is used by pollard-fatorize
    (and (not (zero? a))
         (let-values ([(base n) (simple-as-power a)])
           (if (and (> n 1) (> a 1))
               (list base n)
               #f))))

  (: perfect-power (integer -> (or (list integer integer) false)))
  ;;   if a = b^n with b>1 and n>1
  (define (perfect-power a)
    (and (not (zero? a))
         (let-values ([(base n) (as-power a)])
           (if (and (> n 1) (> a 1))
               (list base n)
               #f))))

  (define (integer-sqrt n)
    (inexact->exact (floor (sqrt n))))

  (: perfect-square (integer -> (or integer false)))
  (define (perfect-square n)
    (let ([sqrt-n (integer-sqrt n)])
      (if (= (* sqrt-n sqrt-n) n)
          sqrt-n
          #f)))

  (: powers-of (integer integer -> (list-of integer)))
  ;;   returns a list of numbers: a^0, ..., a^n
  (define (powers-of a n)
    (let loop ([i 0]
               [a^i 1])
      (if (<= i n)
          (cons a^i (loop (+ i 1) (* a^i a)))
          '())))

  (define prime-divisors/exponents factorize)

  (: prime-divisors (integer -> (list-of integer)))
  ;;   return list of primes in a factorization of n
  (define (prime-divisors n)
    (map car (prime-divisors/exponents n)))

  (: prime-exponents (integer -> (list-of integer)))
  ;;   return list of exponents in a factorization of n
  (define (prime-exponents n)
    (map cadr (prime-divisors/exponents n)))

  (: prime-omega (integer -> integer))
  ;; http://reference.wolfram.com/mathematica/ref/PrimeOmega.html
  (define (prime-omega n)
    (apply +  (prime-exponents n)))

  (: integer-root/remainder (integer integer -> integer integer))
  (define (integer-root/remainder a n)
    (let ([i (integer-root a n)])
      (values i (ensure natural? (- a (expt i n))))))

  (: integer-root (integer integer -> integer))
  (define (integer-root x y)
    ;; y'th root of x
    (cond
     [(eq? x 0) 0]
     [(eq? x 1) 1]
     [(eq? y 1) x]
     [(eq? y 2) (integer-sqrt x)]
     [(not (integer? y))
      (error 'integer-root "internal error (used to return 1 here - why?) remove after testing")]
     [else
      (let ((length (integer-length x)))
        ;; (expt 2 (- length l 1)) <= x < (expt 2 length)
        (ensure
         natural?
         (cond [(<= length y) 1]
               ;; result is >= 2
               [(<= length (* 2 y))
                ;; result is < 4
                (if (< x (expt 3 y)) 2 3)]
               [(even? y) (integer-root (integer-sqrt x) (quotient y 2))]
               [else
                (let* ([length/y/2 ;; length/y/2 >= 1 because (< (* 2 y) length)
                        (quotient (quotient (- length 1) y) 2)])
                  (let ([init-g
                         (let* ([top-bits          (arithmetic-shift x (- (* length/y/2 y)))]
                                [nth-root-top-bits (integer-root top-bits y)])
                           (arithmetic-shift (+ nth-root-top-bits 1) length/y/2))])
                    (let loop ([g init-g])
                      (let* ([a (expt g (ensure natural? (- y 1)))]
                             [b (* a y)]
                             [c (* a (- y 1))]
                             [d (quotient (+ x (* g c)) b)])
                        (let ([diff (- d g)])
                          (cond [(not (negative? diff))
                                 g]
                                [(< diff -1)
                                 (loop d)]
                                [else
                                 ;; once the difference is one, it's more
                                 ;; efficient to just decrement until g^y <= x
                                 (let loop ((g d))
                                   (if (not (< x (expt g y)))
                                       g
                                       (loop (- g 1))))]))))))])))]))


  (: simple-as-power (integer -> integer integer))
  ;;    For a>0 write it as a = b^r where r maximal
  ;;    return (values b r)
  (define (simple-as-power a)
    ;; (displayln (list 'simple-as-power a))
    ;; Note: The simple version is used by pollard-factorize
    (let loop ([n (integer-length a)])
      (let-values ([(root rem) (integer-root/remainder a (add1 n))])
        (if (zero? rem)
            (values root (ensure natural? (add1 n)))
            (if (positive? n)
                (loop (sub1 n))
                (error 'simple-as-power "internal error"))))))

  ;;
  ;; DIVISORS
  ;;

  (: divisors (integer -> (list-of integer)))
  ;;   return the positive divisors of n
  (define (divisors n)
    (cond [(zero? n) '()]
          [else (let ((n+ (if (positive? n) n (- n))))
                  (sort (factorization->divisors (factorize n+)) <))]))

  (: factorization->divisors ((list-of (list integer integer)) -> (list-of integer)))
  (define (factorization->divisors f)
    (cond
     [(null? f) '(1)]
     [else (let ([p (first (first f))]
                 [n (second (first f))]
                 [g (cdr f)])
             ;; f = p^n * g
             (let ([divisors-of-g (factorization->divisors g)])
               (apply append
                      (map
                       (lambda (p^i) (map (lambda (d) (* p^i d)) divisors-of-g))
                       (powers-of p n)))))]))

  ;;
  ;; Number theoretic functions
  ;;

  ;; DEFINITION (Euler's phi function  aka  totient)
  ;;  phi(n) is the number of integers a=1,2,... such that gcd(a,n)=1

  ;; THEOREM
  ;;   If m and n are coprime then
  ;;     phi(mn) = phi(m) phi(n)

  ;; THEOREM (Euler's phi function)
  ;;  If the prime power factorization of p is
  ;;           e1     ek
  ;;     n = p1 ... pk     , where pi is prime and ei>0
  ;;  then
  ;;                   k          1
  ;;   phi(n) = n * product (1 - ---- )
  ;;                  i=1         pi
  (: totient (integer -> integer))
  (define (totient n)
    (let ((ps (prime-divisors n)))
      (ensure natural?
              (* (quotient n (apply * ps))
                 (apply * (map (lambda (p) (sub1 p)) ps))))))

  ;; moebius-mu : natural -> {-1,0-1}
  ;;   mu(n) =  1  if n is a product of an even number of primes
  ;;         = -1  if n is a product of an odd number of primes
  ;;         =  0  if n has a multiple prime factor
  (: moebius-mu (integer -> integer))
  (define (moebius-mu n)
    (define (one? x) (= x 1))
    (define f (factorize n))
    (define exponents (map second f))
    (cond
     [(every one? exponents)
      (let ((primes (map first f)))
        (if (even? (length primes))
            1 -1))]
     [else 0]))


  (: divisor-sum (integer #!optional integer -> integer))
  (define divisor-sum
    ;; returns the sum of the kth power of all divisors of n
    (let ()
      (case-lambda
        [(n) (divisor-sum n 1)]
        [(n k)
         (let* ([f  (factorize n)]
                [ps (map first f)]
                [es (map second f)])
           (define (divisor-sum0 p e) (+ e 1))
           (define (divisor-sum1 p e)
             (let loop ([sum 1]
                        [n 0]
                        [p-to-n 1])
               (cond [(= n e) sum]
                     [else (let ([t (* p p-to-n)])
                             (loop (+ t sum) (+ n 1) t))])))
           (define (divisor-sumk p e)
             (let ([p-to-k (expt p k)])
               (let loop ([sum 1]
                          [n 0]
                          [p-to-kn 1])
                 (cond [(= n e) sum]
                       [else (let ([t (* p-to-k p-to-kn)])
                               (loop (+ t sum) (+ n 1) t))]))))
           (apply * (map (cond [(= k 0) divisor-sum0]
                               [(= k 1) divisor-sum1]
                               [else divisor-sumk])
                         ps es)))])))

  (: mangoldt-lambda (integer -> number))
  (define (mangoldt-lambda n)
    (cond
     [(<= n 0) (error 'mangoldt-lambda "bad argument type - not a positive integer" n)]
     [else (let ((am (prime-power n)))
             (cond
              [(pair? am) (log (car am))]
              [else 0]))])))
