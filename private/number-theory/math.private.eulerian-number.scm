(module math.private.eulerian-number (eulerian-number)
  (import scheme
          (only chicken.base include-relative error add1 sub1)
          (only miscmacros ensure))

  (include-relative "../../utils.scm")

  ;;   computes the Eulerian number <n,k>
  ;;   http://mathworld.wolfram.com/EulerianNumber.html
  (define (eulerian-number* n k)
    ;; Implementation note:
    ;;   Uses standard recurrence : <n,k> = (k+1) <n-1,k> + (n-k) <n-1,k-1>
    ;;   Time: O(n^2)
    (cond [(= k 0) 1]
          [else
           (let ((E (make-vector (max (+ k 1) (+ n 1)) 0)))
             (vector-set! E 0 1) ; <0,0> = 1
             (do ((i 1 (add1 i)))
                 ((> i n))
               (do ((j (- i 1) (sub1 j)))
                   ((= j 0))
                 (vector-set! E j (+ (* (+ j 1) (vector-ref E j))
                                     (* (- i j) (vector-ref E (- j 1)))))))
             (ensure natural? (vector-ref E k)))]))

  (define (eulerian-number n k)
    (cond [(< n 0) (error 'eulerian-number "bad argument type - not a nonnegative integer" n)]
          [(< k 0) (error 'eulerian-number "bad argument type - not a nonnegative integer" k)]
          [else  (eulerian-number* n k)])))
