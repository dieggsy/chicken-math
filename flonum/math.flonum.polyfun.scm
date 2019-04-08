(module math.flonum.polyfun (make-fppolyfun
                             make-even-fppolyfun
                             make-odd-fppolyfun
                             make-quotient-fppolyfun)
  (import scheme
          chicken.flonum)

  (define-syntax horner-iter
    (syntax-rules ()
      [(_ y z ()) y]
      [(_ y z (c0 c ...))
       (horner-iter (fp+ (fp* y z) c0) z (c ...))]))

  (define-syntax make-fppolyfun
    (ir-macro-transformer
     (lambda (e i c)
       (let* ((l (cadr e))
              (rl (reverse l))
              (c0 (car rl))
              (rest (cdr rl)))
         `(lambda (z)
            (horner-iter ,c0 z ,rest))))))

  (define-syntax make-even-fppolyfun
    (syntax-rules ()
      ((_ (c0 c ...))
       (lambda (z)
         ((make-flpolyfun (c0 c ...)) (fp* z z))))))

  (define-syntax make-odd-fppolyfun
    (syntax-rules ()
      ((_ (c0 c ...))
       (lambda (z)
         (fp+ c0 (fp* z ((make-fppolyfun (c ...)) (fp* z z))))))))

  (define-syntax make-quotient-fppolyfun
    (ir-macro-transformer
     (lambda (e i c)
       (let* ((a (cadr e))
              (b (caddr e))
              (a-rev (reverse a))
              (b-rev (reverse b)))
         `(lambda (z)
            (cond ((fp<= (fpabs z) 1.0)
                   (fp/ ((make-fppolyfun ,a) z)
                        ((make-fppolyfun ,b) z)))
                  (else
                   (let ((z (fp/ 1.0 z)))
                     (fp/ ((make-fppolyfun ,a-rev) z)
                          ((make-fppolyfun ,b-rev) z)))))))))))
