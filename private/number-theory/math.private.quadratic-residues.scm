(module math.private.quadratic-residues (quadratic-character
                                         quadratic-residue?
                                         jacobi-symbol)

  (import scheme
          chicken.base
          srfi-1
          math.private.divisibility
          math.private.modular-arithmetic
          math.private.number-theory)

  (include-relative "../../utils.scm")
  ;; DEFINITION (Quadratic residue)
  ;;   a in Un is a quadratic residue,
  ;;   if there exists an s such that a=s^2 (mod n)
  ;;   The number s is called a squre root of a modulo n.

  ;; p is prime
  (define (quadratic-character a p)
    (cond [(< a 0) (error 'quadratic-character "bad argument type - not a nonnegative integer" a)]
          [(<= p 0) (error 'quadratic-character "bad argument type - not a positive integer" p)]
          [else  (let ([l  (modular-expt a (quotient (- p 1) 2) p)])
                   (cond [(or (eqv? l 0) (eqv? l 1))  l]
                         [else  -1]))]))

  (define (quadratic-residue? a n)
    (cond [(< a 0) (error 'quadratic-residue? "bad argument type - not a nonnegative integer" a)]
          [(<= n 0) (error 'quadratic-residue? "bad argument type - not a positive integer" n)]
          [else
           (let* ([ps     (prime-divisors n)]
                  [odd-ps (if (= (first ps) 2)
                              (cdr ps)
                              ps)])
             (and (andmap (lambda (p)
                            (= (quadratic-character a p) 1))
                          odd-ps)
                  (cond
                   [(divides? 8 n)  (= (modulo a 8) 1)]
                   [(divides? 4 n)  (= (modulo a 4) 1)]
                   [else            #t])))]))

  (define (jacobi-symbol a n)
    (unless (odd? n)
      (error 'jacobi-symbol "bad argument type - not an odd integer" n))
    (cond
     [(= n 1) 1]
     [else
      (let ((prime-factors (factorize n)))
        (let next ([factor (first prime-factors)] [remaining-factors (cdr prime-factors)])
          (define qcap (quadratic-character a (first factor)))
          (if (null? remaining-factors)
              (expt qcap (second factor))
              (* (expt qcap (second factor)) (next (first remaining-factors) (cdr remaining-factors)))) ))])))
