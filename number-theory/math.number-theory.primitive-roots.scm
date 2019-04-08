(module math.number-theory.primitive-roots (unit-group
                                      unit-group-order
                                      unit-group-orders
                                      exists-primitive-root?
                                      primitive-root?
                                      primitive-root
                                      primitive-roots)

  (import scheme
          chicken.type
          (only chicken.base include error add1)
          (only chicken.format format)
          (only srfi-1 list-tabulate first filter)
          (only math.number-theory.divisibility coprime?)
          (only math.number-theory.modular-arithmetic
                with-modulus
                mod=
                mod*
                modexpt)
          (only math.number-theory.base
                odd-prime-power?
                totient
                prime-divisors
                prime?
                odd-prime?
                prime-power))

  (include "utils.scm")
  ;; DEFINITION (Order)
  ;;  If G is a finite group with identity element e,
  ;;  then the order of g in G is the least k>0 such that
  ;;  g^k=e

  ;; DEFINITION (Un)
  ;;  The group of units in Zn with respect to multiplication
  ;;  modulo n is called Un.

  (: unit-group (integer -> (list-of integer)))
  (define (unit-group n)
    (cond [(<= n 0) (error 'unit-group "bad argument type - not a positive integer" n)]
          [else  (filter (lambda (m) (coprime? m n))
                         (list-tabulate (- n 1) add1))]))

  (: unit-group-order (integer integer -> integer))
  (define (unit-group-order g n)
    (cond [(<= g 0) (error 'unit-group-order "bad argument type - not a positive integer" g)]
          [(<= n 0) (error 'unit-group-order "bad argument type - not a positive integer" n)]
          [(not (coprime? g n))
           (error 'unit-group-order (format "expected coprime arguments; given ~A and ~A" g n))]
          [else
           (with-modulus n
                         (let loop ([k 1]
                                    [a g])
                           (cond [(mod= a 1)  k]
                                 [else  (loop (+ k 1) (mod* a g))])))]))

  (: unit-group-orders (integer -> (list-of integer)))
  (define (unit-group-orders n)
    (cond [(<= n 0) (error 'unit-group-orders "bad argument type - not a positive integer" n)]
          [else  (map (lambda (m) (unit-group-order m n))
                      (unit-group n))]))

  ;; DEFINITION (Primitive Root)
  ;;  A generator g of Un is called a primitive root mod n.
  ;;  I.e.  order(g)=phi(n)  <=>  g is primitive

  #;
  (define (primitive-root? g n)
  (if (not (coprime? g n))
  (error 'primitive-root? "expected coprime arguments; given ~e and ~e" g n)
  (= (unit-group-order g n) (phi n))))

  ;; THEOREM (Existence of primitive roots)
  ;;      Un is cyclic   (i.e. have a primitive root)
  ;;  <=> n = 1, 2, 4, p^e, 2*p^e  where p is an odd prime

  (: exists-primitive-root? (integer -> boolean))
  (define (exists-primitive-root? n)
    (cond [(<= n 0) (error 'exists-primitive-root? "bad argument type - not a positive integer" n)]
          [(or (= n 1) (= n 2) (= n 4))  #t]
          [(odd? n)  (odd-prime-power? n)]
          [else      (odd-prime-power? (quotient n 2))]))

  ;; LEMMA
  ;;       a in Un is a primitive root
  ;;  <=>   phi(n)/q
  ;;       a         <> 1  in Un for all primes q dividing phi(n)

  (: primitive-root? (integer integer -> boolean))
  (define (primitive-root? g n)
    (cond [(<= g 0)  (error 'primitive-root? "bad argument type - not a positive integer" g)]
          [(<= n 0)  (error 'primitive-root? "bad argument type - not a positive integer" n)]
          [(not (coprime? g n))
           (error 'primitive-root? "expected coprime arguments; given ~e and ~e" g n)]
          [else
           (let ((phi-n (totient n)))
             (with-modulus n
                           (andmap (lambda (x) x)
                                   (map (lambda (q) (not (mod= (modexpt g (quotient phi-n q)) 1)))
                                        (prime-divisors phi-n)))))]))

  ;; primitive-root : N -> Un
  ;;  return primitive root of n if one exists,
  ;;  otherwise return #f
  #;
  (define (primitive-root n)
  (and (exists-primitive-root? n)
  (let* ([phi-n (phi n)]
  [qs    (prime-divisors phi-n)])
  (define (primitive-root? g)
  (with-modulus n (andmap (lambda (x) x)
  (map (lambda (q)
  (not (= (expt g (/ phi-n q)) 1)))
  qs))))
  (let loop ([g 1])
  (cond
  [(= g n)                #f]
  [(not (coprime? g n))   (loop (+ g 1))]
  [(primitive-root? g)    g]
  [else                   (loop (+ g 1))])))))

  ;; LEMMA
  ;;  If Un has a primitive root, then it has phi(phi(n)) primitive roots

  ;; primitive-roots : integer -> list
  ;;  return list of all primitive roots of Un
  (: primitive-roots (integer -> (list-of integer)))
  (define (primitive-roots n)
    (cond [(<= n 0)  (error 'primitive-roots "bad argument type - not a positive integer" n)]
          [(not (exists-primitive-root? n))  '()]
          [else
           (let* ([phi-n (totient n)]
                  [qs    (prime-divisors phi-n)])
             (define (primitive-root? g)
               (with-modulus n
                             (andmap (lambda (x) x)
                                     (map (lambda (q)
                                            (not (mod= (modexpt g (quotient phi-n q)) 1)))
                                          qs))))
             (let loop ([g 1]
                        [roots '()])
               (cond
                [(= g n)                (reverse roots)]
                [(not (coprime? g n))   (loop (+ g 1)  roots)]
                [(primitive-root? g)    (loop (+ g 1) (cons g roots))]
                [else                   (loop (+ g 1)  roots)])))]))

  (: primitive-root (integer -> (or integer false)))
  (define (primitive-root n)
    (cond [(<= n 0)  (error 'primitive-root "bad argument type - not a positive integer" n)]
          [(not (exists-primitive-root? n))  #f]
          ;; U_p^e , p odd
          [(and (odd-prime-power? n) (not (prime? n)))
           (let* ((pp (prime-power n))
                  (p (if pp (first pp) (error 'primitive-root "internal error")))
                  (gg (primitive-root p))
                  (g (or gg (error 'primitive-root "internal error"))))
             (if (= (unit-group-order g (* p p)) (totient (* p p)))
                 g
                 (modulo (+ g p) n)))]
          ;; U_2p^e , p odd
          [(and (even? n) (odd-prime? (quotient n 2)))
           (let* ((gg (primitive-root (quotient n 2)))
                  (g (or gg (error 'primitive-root "internal error"))))
             (if (odd? g)
                 g
                 (modulo (+ g (quotient n 2)) n)))]
          ;; General case
          [else
           (let* ((phi-n (totient n))
                  (qs    (prime-divisors phi-n)))
             (define (primitive-root? g)
               (with-modulus n
                             (andmap (lambda (x) x)
                                     (map (lambda (q)
                                            (not (mod= (modexpt g (quotient phi-n q)) 1)))
                                          qs))))
             (let loop ([g 1])
               (cond [(= g n)                #f]
                     [(not (coprime? g n))   (loop (+ g 1))]
                     [(primitive-root? g)    g]
                     [else                   (loop (+ g 1))])))])))
