(module math.base.random (random-bits
                          random-natural
                          random-integer)
  (import scheme
          chicken.type
          chicken.base
          chicken.bitwise
          chicken.fixnum
          chicken.random
          math.racket-shim)

  ;; Random bits are taken in blocks of this size:
  (define block-bits 29)
  (define block-size (arithmetic-shift 1 block-bits))

  (: random-bits (integer -> integer))
  (define (random-bits bits)
    (cond [(<= bits 0) (error 'random-bits "bad argument type - not a positive integer" bits)]
          [else
           (let ((max-blocks (quotient bits block-bits))
                 (rem-bits (remainder bits block-bits)))
             (let loop ([blocks 0]
                        [r (random (fxshl 1 rem-bits))])
               (assume ((blocks fixnum)
                        (r integer))
                 (cond [(fx< blocks max-blocks)
                        (loop (fx+ blocks 1)
                              (bitwise-ior (arithmetic-shift r block-bits)
                                           (random block-size)))]
                       [else  r]))))]))

  (define random-max 4294967087)
  (define bias-bits (* 2 block-bits))

  (define bias-bits (* 2 block-bits))

  (: random-natural (integer -> integer))
  ;; Returns a random integer in the interval [0..n)
  (define (random-natural n)
    (cond
     [(<= n 0)
      (error 'random-natural "bad argument type - not a positive integer" n)]
     [(<= n random-max)  (random n)]
     [else
      ;; Rejection sampling has rejection probability approaching 1/2 in the worst cases; that is,
      ;; when n = 1+2^i for some large-ish integer i
      ;; Adding extra bits shrinks the rejection probability to near zero (it approaches
      ;; (* 1/2 (expt 2 (- bias-bits)))), at the cost of some bias
      ;; The bias starts become detectable after taking (expt 2 bias-bits) samples, which is plenty
      (let* ((bits (+ bias-bits (integer-length (- n 1))))
             (m (arithmetic-shift 1 bits)))
        (let loop ()
          (define r (quotient (* (+ (random-bits bits) 1) n) m))
          (if (>= r n) (loop) r)))]))

  (: random-integer (integer integer -> integer))
  (define (random-integer a b)
    (let ([a  (min a b)] [b  (max a b)])
      (+ a (random-natural (- b a))))))
