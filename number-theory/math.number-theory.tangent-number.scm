(module math.number-theory.tangent-number (tangent-number)

  (import scheme
          chicken.type
          (only chicken.base error add1 sub1))

  (: tangent-number (integer -> integer))
  ;;  The n'th tangent number:
  ;;  <http://mathworld.wolfram.com/TangentNumber.html>
  (define (tangent-number n)
    (cond [(< n 0) (error 'tangent-number "bad argument type - not a nonnegative integer" n)]
          [else
           ;; Implementation note:
           ;;   See "Concrete Mathematics" p 287 for the method
           (let ((T (make-vector (+ n 2) 0)))
             ;; T[x]=x
             (vector-set! T 1 1)
             (do ([k 0 (add1 k)])
                 ([> k n])
               ;; differentiate T[x]
               (do ([i 0 (add1 i)])
                   ([> i k])
                 (vector-set! T i (* (add1 i) (vector-ref T (add1 i)))))
               (vector-set! T k 0)
               ;; multiply T[x] with 1+x^2
               (do ([i (+ k 1) (sub1 i)])
                   ([= i 1])
                 (vector-set! T i (+ (vector-ref T i) (vector-ref T (- i 2))))))
             (vector-ref T 0))])))
