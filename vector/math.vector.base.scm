(module math.vector.base (vector-ref!)
  (import scheme
          chicken.base
          chicken.fixnum)
  (define vector-ref!
    (case-lambda
      [(vs i thnk)  (vector-ref! vs i thnk not)]
      [(vs i thnk nothing?)
       (define n (vector-length vs))
       (define v (vector-ref vs i))
       (if (nothing? v)
           (let ([v  (thnk)])
             (vector-set! vs i v)
             v)
           v)])))
