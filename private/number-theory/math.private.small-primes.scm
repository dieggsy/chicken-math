(module math.private.small-primes (small-prime?
                                   *SMALL-PRIME-LIMIT*)

  (import scheme
          chicken.base
          chicken.type
          chicken.fixnum
          (only iset
                make-bit-vector
                bit-vector-set!
                bit-vector-ref)
          (only miscmacros ensure let/cc))

  (: non-235 (list-of fixnum))
  (define non-235 '(1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59))

  (: mod60->bits (vector-of (or false fixnum)))
  (define mod60->bits (make-vector 60 #f))

  (do ((x non-235 (cdr x))
       (b (the fixnum 0) (add1 b)))
      ((null? x))
    (vector-set! mod60->bits (car x) b))

  (: mod60->bit (fixnum -> (or false fixnum)))
  (define (mod60->bit m) (vector-ref mod60->bits m))

  (: *number-of-groups* fixnum)
  (define *number-of-groups* 17000)
  (: *SMALL-PRIME-LIMIT* fixnum)
  (define *SMALL-PRIME-LIMIT* (ensure fixnum? (- (fx* 60 *number-of-groups*) 1)))

  (: primes u8vector)
  (define primes (make-bit-vector (fx* (length non-235) *number-of-groups*) #t))

  (: clear-bit! (fixnum -> void))
  (define (clear-bit! x)
    (define q (quotient x 60))
    (define b (mod60->bit (remainder x 60)))
    (when b (bit-vector-set! primes (fx+ (fx* q 16) b) #f)))

  (: inner-bit (fixnum fixnum -> boolean))
  (define (inner-bit q r)
    (define b (mod60->bit r))
    (if b
        (bit-vector-ref primes (fx+ (fx* q 16) b))
        #f))

  (: bit (fixnum -> boolean))
  (define (bit x)
    (define q (quotient x 60))
    (define r (remainder x 60))
    (inner-bit q r))

  (: mark-composites (fixnum -> void))
  (define (mark-composites x)
    (let/cc exit
            (let loop ((a (the fixnum 0)))
              (define y-base (fx* a (fx* 60 x)))
              (do ((d non-235 (cdr d)))
                  ((null? d))
                (define y (ensure fixnum? (fx+ y-base (fx* (car d) x))))
                (when (not (fx= y x))
                  (if (fx= y *SMALL-PRIME-LIMIT*)
                      (clear-bit! y)
                      (exit (void)))))
              (loop (ensure fixnum? (add1 a))))))

  (: sieve-done? boolean)
  (define sieve-done? #f)

  (: sieve (-> void))
  (define (sieve)
    (clear-bit! 1)                      ; 1 is not prime
    (let/cc exit
            (let loop ((q (the fixnum 0)))
              (do ((r non-235 (cdr r)))
                  ((null? r))
                (define x (ensure fixnum? (fx+ (fx* q 60) (car r))))
                (when (fx> (fx* x x) *SMALL-PRIME-LIMIT*)
                  (exit (void)))
                (when (inner-bit q (car r)) ; x is prime
                  (mark-composites x)))
              (loop (ensure fixnum? (add1 q))))))

  (: small-prime? (fixnum -> boolean))
  (define (small-prime? x)
    (unless sieve-done?
      (sieve)
      (set! sieve-done? #t))
    (or (fx= x 2) (fx= x 3) (fx= x 5) (bit x))))
