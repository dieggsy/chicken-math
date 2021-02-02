(import srfi-1)

(test-group "math.number-theory"

  (import math.number-theory)

  (test-group "math.private.quadratic"
    (test '(-2 2)      (complex-quadratic-solutions 1 0 -4))
    (test '(0.0-2i 0.0+2.0i) (complex-quadratic-solutions 1 0 +4))
    (test '(0)         (complex-quadratic-solutions 1 0 0))
    (test '(-2 2)      (quadratic-solutions 1 0 -4))
    (test '()          (quadratic-solutions 1 0 +4))
    (test '(0)         (quadratic-solutions 1 0 0))
    (test '(-2 2)      (quadratic-integer-solutions 1 0 -4))
    (test '()          (quadratic-integer-solutions 1 0 +4))
    (test '(0)         (quadratic-integer-solutions 1 0 0))
    (test '(2)         (quadratic-natural-solutions 1 0 -4))
    (test '()          (quadratic-natural-solutions 1 0 +4))
    (test '(0)         (quadratic-natural-solutions 1 0 0)))

  (test-group "math.number-theory.eulerian-number"
    (test '(1 26 66 26 1)
          (map (lambda (x) (eulerian-number 5 x)) '(0 1 2 3 4))))

  (test-group "math.number-theory.primitive-roots"
    (test '(1 3 7 9 11 13 17 19) (unit-group 20))  ; 19 !!!!
    (test 2 (unit-group-order 19 20))
    (test 4 (unit-group-order  3 20))
    (test '(1 4 4 2 2 4 4 2) (unit-group-orders 20)) ; (unit-group-order 3 20)=4, ...
    (test-assert   (andmap exists-primitive-root? '(1 2 4 3 9 6 18)))
    (test-assert (not  (ormap  exists-primitive-root? '(8 16 12))))
    (test #f (primitive-root 20))
    (test 7 (primitive-root 10)) ; (length (unit-group 10)) = (unit-group-order 7 10)
    (test-assert   (primitive-root? 7 10))
    (test-assert (not  (primitive-root? 7 20)))
    (test '(3 7) (primitive-roots 10))
    (define (find-and-check-root n)
      (define r (primitive-root n))
      (cond [(not r) #t]
            [else (= (length (unit-group n)) (unit-group-order r n))]))
    (test-assert  (andmap find-and-check-root '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 78125))))

  (test-group "math.number-theory.polygonal"
    (test '(0 1 3  6 10 15) (map triangle-number    '(0 1 2 3 4 5)))
    (test '(0 1 5 12 22 35) (map pentagonal-number  '(0 1 2 3 4 5)))
    (test '(0 1 6 15 28 45) (map hexagonal-number   '(0 1 2 3 4 5)))
    (test '(0 1 7 18 34 55) (map heptagonal-number  '(0 1 2 3 4 5)))
    (test '(0 1 8 21 40 65) (map octagonal-number   '(0 1 2 3 4 5)))
    (test-assert   (andmap triangle-number?    '(0 1 3  6 10 15)))
    (test-assert   (andmap square-number?      '(0 1 4  9 16 25)))
    (test-assert   (andmap pentagonal-number?  '(0 1 5 12 22 35)))
    (test-assert   (andmap hexagonal-number?   '(0 1 6 15 28 45)))
    (test-assert   (andmap heptagonal-number?  '(0 1 7 18 34 55)))
    (test-assert   (andmap octagonal-number?   '(0 1 8 21 40 65))))

  (test-group "math.number-theory.farey"
    (test '(0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1) (farey-sequence 5))
    (test 2/3 (mediant 1/1 1/2)))

  (test-group "math.number-theory.fibonacci"
    (test '(0 1 1 2 3 5 8 13) (list-tabulate 8 fibonacci))
    (test '(2 1 3 4 7 11 18 29) (list-tabulate 8 (make-fibonacci 2 1)))
    (do ((a -5 (add1 a)))
        ((= a 6))
      (do ((b -5 (add1 b)))
          ((= b 6))
        (do ((mod 1 (add1 mod)))
            ((= mod 8))
          (test-assert (equal? (list-tabulate 20 (lambda (n) ((make-modular-fibonacci a b) n mod)))
                               (list-tabulate 20 (lambda (n) (modulo ((make-fibonacci a b) n) mod)))))))))

  (test-group "math.number-theory.partitions"
    (test '(1 1 2 3 5 7 11 15 22 30 42)
          (map partitions '(0 1 2 3 4 5 6 7 8 9 10))))


  (test-group "math.number-theory.bernoulli"
    (test '(1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66 0 -691/2730 0 7/6 0 -3617/510 0 43867/798)
          (map bernoulli-number '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))))

  (test-group "math.number-theory.tangent-number"
    (test '(1 2 16 272 7936 353792 22368256) (map tangent-number '(1 3 5 7 9 11 13)))
    (test '(0 0 0 0 0 0) (map tangent-number '(0 2 4 6 8 10))))

  (test-group "math.number-theory.factorial"
    (define fact-table-size 171)
    (define simple-cutoff 244)

    (define (test-factorial n)
      (if (= n 0) 1 (* n (test-factorial (- n 1)))))

    (test '(1 1 2 6 24 120) (map factorial (list 0 1 2 3 4 5)))
    (test-assert (= (factorial (+ fact-table-size 1)) (test-factorial (+ fact-table-size 1))))
    (test-assert (= (factorial (+ simple-cutoff 1)) (test-factorial (+ simple-cutoff 1))))

    (test 720 (permutations 10 3))
    (test 1 (permutations 10 0))
    (test 3628800 (permutations 10 10))
    (test 1 (permutations 0 0))

    (test 3491888400 (multinomial 20 '(3 4 5 8)))
    (test 1 (multinomial 0 '()))
    (test 0 (multinomial 4 '(1 1))))

  (test-group "math.number-theory.binomial"
    (test 120 (binomial 10 3))
    (test 0 (binomial 10 11))
    (test 1 (binomial 10 0))
    (test 1 (binomial 10 10))
    (test 10 (binomial 10 1))
    (test 10 (binomial 10 9)))


  (test-group "math.number-theory.base"
    (test-assert  (divides? 2 12))
    (test-assert (not (divides? 2 13)))
    (test-assert  (divides? 2 0))
    ;; (check-exn   (divides? 0 2)) ?

    (test 3 (max-dividing-power 3 27))
    (test 3 (max-dividing-power 3 (* 27 2)))

    (define (list-dot as bs)
      (if (null? as)
          0
          (+ (* (car as) (car bs)) (list-dot (cdr as) (cdr bs)))))

    (define (member? x xs)
      (not (not (member x xs))))

    (test-assert ; 2*12-1*20 = 4 = gcd(12,20)
        (= (list-dot '(12 20) (bezout 12 20)) (gcd 12 20)))
    (test-assert (= (list-dot '(12 20) (bezout 12 20)) (gcd 12 20)))
    (test-assert (= (list-dot '(20 16) (bezout 20 16)) (gcd 20 16)))
    (test-assert (= (list-dot '(12 20 16) (bezout 12 20 16)) (gcd 12 20 16)))

    (test-assert (coprime? (* 3 7) (* 5 19)))
    (test-assert (not (coprime? (* 3 7 5) (* 5 19 2))))

    (test-assert  (pairwise-coprime? 10 7 33 13))
    (test-assert (not (pairwise-coprime? 10 7 33 14)))
    (test-assert (not (pairwise-coprime? 6 10 15)))
    (test-assert  (coprime? 6 10 15))
    (define (check-inverse n)
      (define m (and (coprime? n 20) (modular-inverse n 20)))
      (cond [m  (= (remainder (* n m) 20) 1)]
            [else  #t]))
    (test-assert (andmap check-inverse (list-tabulate 20 (lambda (x) (+ x 1)))))

    (test 23 (solve-chinese '(2 3 2) '(3 5 7)))


    (test '(1 2 3 4 6 12)  (divisors 12))
    (test '(1 2 3 4 6 12) (divisors -12))
    (test '()   (divisors 0))

    (test '(-3 -2 2 3 5 7 11 13 17 19) (next-primes -5 10))
    (test '(3 2 -2 -3 -5 -7 -11 -13 -17 -19) (prev-primes  5 10))
    (test 2 (next-prime 0))
    (test 2 (next-prime 1))
    (test 7 (prev-prime 10))
    (test 7 (prev-prime 8))
    (test 13 (prev-prime 17))
    (test 2 (nth-prime 0))
    (test 3 (nth-prime 1))
    (test 5 (nth-prime 2))


    (let ()
      (import chicken.format)
      (define (prime-sum start delta)
        (define s 0)
        (do ([i start (add1 i)])
            ([= i (+ start delta 1)])
          (set! s (+ s (next-prime i))))
        s)
      (test 1001511919 (prime-sum (expt 10 6) 1000))    ; Sum[NextPrime[n], {n, 10^6, 10^6 + 1000}]
      (test 10010514423 (prime-sum (expt 10 7) 1000))   ; Sum[NextPrime[n], {n, 10^7, 10^7 + 1000}]
      (test 100100519271 (prime-sum (expt 10 8) 1000))  ; Sum[NextPrime[n], {n, 10^8, 10^8 + 1000}]
      (test 1001000516807 (prime-sum (expt 10 9) 1000)) ; Sum[NextPrime[n], {n, 10^9, 10^9 + 1000}]
      )

    #;(test 7472966967499
    (let ()
    ;; sum of the first million primes ; ;
    (define: s : Integer 0)
    (define: p : Integer 1)
    (for: ([i (in-range 0 (expt 10 6))])
    (when (zero? (remainder i 10000))
    (newline)
    (display i))
    (when (zero? (remainder i 1000))
    (display "."))
    (set! p (next-prime p))
    (set! s (+ s p)))
    s))

    (test 10000019 (next-prime (expt 10 7)))
    (test 100000007 (next-prime (expt 10 8)))
    (test-assert (equal? (factorize (* 10000019 100000007)) '((10000019 1) (100000007 1))))
    (define (check-factorize n)
      (= (defactorize (factorize n)) n))
    (test-assert
        (let loop ((n (expt 10 9)))
          (if (= n (+ (expt 10 9) 10000))
              #t
              (and (check-factorize n) (loop (add1 n))))))

    (define (check-as-power a r n)
      (define-values (b e) (as-power a))
      (and (= b r) (= e n)))
    (test-assert (check-as-power 27 3 3))
    (test-assert (check-as-power 28 28 1))
    (test-assert (check-as-power (* 5 5 7 7 7) (* 5 5 7 7 7) 1))
    (test-assert (check-as-power (* 5 5 7 7 7 7) 245 2))

    (test-assert (prime-power? (expt 3 7)))
    (test-assert (not (prime-power? (expt 12 7))))

    (test-assert (not (perfect-power? 3)))
    (test-assert (perfect-power? 9))
    (test-assert (perfect-power? (expt 12 7)))
    (test-assert (not (perfect-power? (- (expt 12 7) 1))))


    (test 1 (moebius-mu (* 3 5 7 11)))
    (test -1    (moebius-mu (* 3 5 7)))
    (test 0  (moebius-mu (* 3 5 5 7)))

    (test 2340   (divisor-sum 1000))
    (test 16 (divisor-sum 1000 0))
    (test 2340 (divisor-sum 1000 1))
    (test 1383460 (divisor-sum 1000 2))

    (define (check-integer-root a n)
      (define r (integer-root a n))
      (unless (and (<= (expt r n) a) (> (expt (+ r 1) n) a))
        (print (list 'check-integer-root 'a a 'n n)))
      (and (<= (expt r n) a) (> (expt (+ r 1) n) a)))

    (let ((a-limit (+ (expt 10 9) 10000)))
      (do ((a (expt 10 9) (add1 a))
           (n 2 (add1 n)))
          ((or (= n 5) (= a a-limit)))
        (test-assert (check-integer-root a n))))
    )

  (test-group "math.number-theory.quadratic-residues"
    (test -1 (quadratic-character  2 5))
    (test -1 (quadratic-character  3 5))
    (test 0  (quadratic-character  5 5))
    (test -1 (quadratic-character  7 5))
    (test 1  (quadratic-character 11 5))

    (test 0 (jacobi-symbol 0 23))
    (test 1 (jacobi-symbol 1 1))
    (test 1 (jacobi-symbol 2 1))
    (test 1 (jacobi-symbol 4 1))
    (test -1 (jacobi-symbol 2 3))
    (test 1 (jacobi-symbol 4 5))
    (test -1 (jacobi-symbol 7 5))
    (test -1 (jacobi-symbol 5 3))
    (test 1 (jacobi-symbol 25 53))
    (test 1 (jacobi-symbol 21 1))
    (test 0 (jacobi-symbol 21 21))
    (test 0 (jacobi-symbol 12 3))
    (test -1 (jacobi-symbol 30 59))
    (test -1 (jacobi-symbol 7 51))
    (test 0 (jacobi-symbol 22 55))

    (test-assert  (quadratic-residue? 1 17))
    (test-assert  (quadratic-residue? 2 17))
    (test-assert  (quadratic-residue? 4 17))
    (test-assert  (quadratic-residue? 8 17))
    (test-assert  (quadratic-residue? 9 17))
    (test-assert  (quadratic-residue? 13 17))
    (test-assert  (quadratic-residue? 15 17))
    (test-assert  (quadratic-residue? 16 17))
    (test-assert (not (quadratic-residue?  3 17)))
    (test-assert (not (quadratic-residue?  5 17)))
    (test-assert (not (quadratic-residue?  6 17)))
    (test-assert (not (quadratic-residue?  7 17)))
    (test-assert (not (quadratic-residue? 10 17)))
    (test-assert (not (quadratic-residue? 11 17)))
    (test-assert (not (quadratic-residue? 12 17)))
    (test-assert (not (quadratic-residue? 14 17)))))
