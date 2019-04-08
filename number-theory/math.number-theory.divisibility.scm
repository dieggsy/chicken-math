(module math.number-theory.divisibility (divides?
                                         coprime?
                                         pairwise-coprime?
                                         bezout)
  (import scheme
          chicken.type
          (only chicken.base
                include
                error
                quotient&remainder
                let-values
                exact-integer?))

  (include "utils.scm")

  (: divides? (integer integer -> boolean))
  (define (divides? a b)
    (if (zero? a) #f
        (= (remainder b a) 0)))

  (: coprime? (integer #!rest integer -> boolean))
  (define (coprime? a . bs)
    (= 1 (apply gcd (cons a bs))))

  (: pairwise-coprime? (integer #!rest integer -> boolean))
  (define (pairwise-coprime? a . bs)
    (or (null? bs)
        (and
         (andmap (lambda (b) (coprime? a b)) bs)
         (apply pairwise-coprime? bs))))


  (: bezout-binary (integer integer -> (list integer integer)))
  (define (bezout-binary a b)
    (: loop (integer integer integer integer integer integer -> (list integer integer)))
    (define (loop a b ua va ub vb)  ; a>=b>0 , a = ua*a+ub*b,  b = ub*a+ub*b
      (let-values ([(q r) (quotient&remainder a b)])
        (if (= r 0)
            (list ub vb)
            (loop b r ub vb (- ua (* q ub)) (- va (* q vb))))))
    (: start (integer integer -> (list integer integer)))
    (define (start a b)
      (if (> a b)
          (loop a b 1 0 0 1)
          (loop b a 0 1 1 0)))
    (cond [(and (positive? a) (positive? b))
           (start a b)]
          [(and (negative? a) (negative? b))
           (let ((uv (start (- a) (- b))))
             (list (- (car uv)) (- (cadr uv))))]
          [(and (negative? a) (positive? b))
           ;; choose k s.t. a+kb>0
           (let* ((k (+ (quotient (- a) b) 1))
                  (uv (start (+ a (* k b)) b))
                  (u (car uv)) (v (cadr uv)))
             (list u (+ (* u k) v)))]
          [(and (positive? a) (negative? b))
           ;; choose k s.t. ak+b>0
           (let* ((k (+ (quotient (- b) a) 1))
                  (uv (start a (+ (* k a) b)))
                  (u (car uv)) (v (cadr uv)))
             (list (+ u (* k v)) v))]
          [else (error "Internal error in bezout-binary")]))

  (: bezout (integer #!rest integer -> (list-of integer)))
  (define (bezout a . bs)
    (cond
     [(null? bs)        (list 1)]
     [(null? (cdr bs))  (bezout-binary a (car bs))]
     [else
      (let ([uvs (apply bezout bs)]
            [st  (bezout-binary (apply gcd bs) a)])
        (let ([s (car st)]
              [t (cadr st)])
          (cons t (map (lambda (u) (* s u))
                       uvs))))])))
