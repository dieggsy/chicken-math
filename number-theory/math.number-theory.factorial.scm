(module math.number-theory.factorial (factorial
                                permutations
                                multinomial)

  (import scheme
          (only chicken.base include error add1 fixnum?)
          (only chicken.fixnum fx*)
          (only srfi-1 list-tabulate find fold)
          (only miscmacros ensure))

  (include "utils.scm")

  (define fact-table-size 171)

  (define fact-table
    (list->vector
     (reverse
      (fold (lambda (n ns)
              (cons (* n (car ns)) ns))
            '(1)
            (list-tabulate (- fact-table-size 1) add1)))))

  (define simple-cutoff 244)

  (define (factorial-simple n)
    (cond [(< n fact-table-size) (vector-ref fact-table n)]
          [else (* n (factorial-simple (- n 1)))]))

  (define (factorial n)
    (cond [(negative? n) (error 'factorial "bad argument type - not a nonnegative integer" n)]
          [(not (fixnum? n)) (error 'factorial "bad argument type - not a nonnegative fixnum" n)]
          [(eqv? n 0)  1]
          [(eqv? n 1)  1]
          [(< n simple-cutoff)  (factorial-simple n)]
          [else
           (let loop ([n n]
                      [m 1])
             (define n-m (- n m))
             (cond [(<= n-m 0)  n]
                   [else  (let ((2m (fx* m 2)))
                            (* (loop n 2m) (loop n-m 2m)))]))]))

  (define (permutations n k)
    (cond [(negative? n) (error 'permutations "bad argument type - not a nonnegative integer" n)]
          [(negative? k) (error 'permutations "bad argument type - not a nonnegative integer" k)]
          [(zero? k)  1]
          [(> k n)  0]
          [else  (ensure natural?
                         (/ (factorial n) (factorial (- n k))))]))

  (define (multinomial n ks)
    (cond [(negative? n) (error 'multinomial "bad argument type - not a nonnegative integer" n)]
          [(find negative? ks) (error 'multinomial "bad argument type - not a list of nonnegative integers" ks)]
          [(not (= n (apply + ks)))  0]
          [else  (ensure natural?
                         (apply / (factorial n) (map factorial ks)))]))
  )