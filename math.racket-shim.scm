(module math.racket-shim *
  (import scheme
          chicken.flonum
          chicken.foreign
          (only chicken.base conjoin complement exact-integer?
                case-lambda error unless sub1 flonum?)
          r6rs.bytevectors
          chicken.fixnum
          chicken.random
          (only chicken.bitwise arithmetic-shift bitwise-and)
          (only miscmacros ensure)
          (only chicken.platform machine-byte-order))

  (define natural? (conjoin (complement negative?) exact-integer?))

  (define (andmap fn ls0)
    (let mapf ((ls ls0))
      (or
       (null? ls)
       (and
        (fn (car ls))
        (mapf (cdr ls))))))

  (define (ormap func ls0 . rest)
    (and
     (pair? ls0)
     (let ((rest (cons ls0 rest)))
       (or
        (apply func (map car rest))
        (apply ormap func (map cdr rest))))))


  (define (system-big-endian?)
    (eqv? (machine-byte-order) 'big-endian))

  (define bytes? bytevector?)

  (define bytes-length bytevector-length)

  (define real->floating-point-bytes
    (case-lambda
      [(num size big-endian? bstr start)
       (ensure bytevector? bstr)
       (case size
         [(4)
          (bytevector-ieee-single-set! bstr start num (if big-endian?
                                                          (endianness big)
                                                          (endianness little)))]
         [(8)
          (bytevector-ieee-double-set! bstr start num (if big-endian?
                                                          (endianness big)
                                                          (endianness little)))]
         [else
          (error 'real->floating-point-bytes "bad argument type - not 4 or 8" size)])
       bstr]
      [(num size)
       (real->floating-point-bytes num size (system-big-endian?)
                                   (and (exact-integer? size) (<= 2 size 8) (make-bytevector size)) 0)]
      [(num size big-endian?)
       (real->floating-point-bytes num size big-endian?
                                   (and (exact-integer? size) (<= 2 size 8) (make-bytevector size)) 0)]
      [(num size big-endian? bstr)
       (real->floating-point-bytes num size big-endian? bstr 0)]))

  (define floating-point-bytes->real
    (case-lambda
      [(bstr big-endian? start end)
       (ensure bytes? bstr)
       (ensure natural? start)
       (ensure natural? end)
       (case (- end start)
         [(4)
          (bytevector-ieee-single-ref bstr start (if big-endian?
                                                     (endianness big)
                                                     (endianness little)))]
         [(8)
          (bytevector-ieee-double-ref bstr start (if big-endian?
                                                     (endianness big)
                                                     (endianness little)))]
         [else
          (error 'integer-bytes->integer "length is not 4 or 8 bytes"
                 `(- ,end ,start)
                 (- end start))])]
      [(bstr)
       (floating-point-bytes->real bstr (system-big-endian?) 0 (and (bytes? bstr) (bytes-length bstr)))]
      [(bstr big-endian?)
       (floating-point-bytes->real bstr big-endian? 0 (and (bytes? bstr) (bytes-length bstr)))]
      [(bstr big-endian? start)
       (floating-point-bytes->real bstr big-endian? start (and (bytes? bstr) (bytes-length bstr)))]))

  (define integer->integer-bytes
    (case-lambda
      [(num size signed? big-endian? bstr start)
       (let ([check (lambda (n lo hi)
                      (ensure bytes? bstr)
                      (ensure natural? start)
                      (let ([len (bytevector-length bstr)])
                        (unless (>= len n)
                          (error 'integer->integer-bytes
                                 "destination bytevector is too small for number of bytes"
                                 len n))
                        (unless (<= start (- len n))
                          (error 'integer->integer-bytes
                                 "starting position is too large given bytevector length and number of bytes" start len n))
                        (unless (<= lo num hi)
                          (error 'integer->integer-bytes
                                 (string-append "number is out of bounds for size in "
                                                (if signed?
                                                    "signed"
                                                    "unsigned")
                                                " bytes")
                                 num
                                 n))))])
         (case size
           [(1)
            (if signed?
                (check 1 -128 127)
                (check 1 0 255))
            (if signed?
                (bytevector-s8-set! bstr start num)
                (bytevector-u8-set! bstr start num))]
           [(2)
            (if signed?
                (check 2 -32768 32767)
                (check 2 0 65535))
            (if signed?
                (bytevector-s16-set! bstr start num (if big-endian?
                                                        (endianness big)
                                                        (endianness little)))
                (bytevector-u16-set! bstr start num (if big-endian?
                                                        (endianness big)
                                                        (endianness little))))]
           [(4)
            (if signed?
                (check 4 -2147483648 2147483647)
                (check 4 0 8589934591))
            (if signed?
                (bytevector-s32-set! bstr start num (if big-endian?
                                                        (endianness big)
                                                        (endianness little)))
                (bytevector-u32-set! bstr start num (if big-endian?
                                                        (endianness big)
                                                        (endianness little))))]
           [(8)
            (if signed?
                (check 8 -9223372036854775808 9223372036854775807)
                (check 8 0 18446744073709551615))
            (if signed?
                (bytevector-s64-set! bstr start num (if big-endian?
                                                        (endianness big)
                                                        (endianness little)))
                (bytevector-u64-set! bstr start num (if big-endian?
                                                        (endianness big)
                                                        (endianness little))))]
           [else
            (error 'integer->integer-bytes "size must be 1, 2, 4, or 8" size)]))
       bstr]
      [(num size signed?)
       (integer->integer-bytes num size signed? (system-big-endian?)
                               (and (exact-integer? size) (<= 1 size 8) (make-bytevector size)) 0)]
      [(num size signed? big-endian?)
       (integer->integer-bytes num size signed? big-endian?
                               (and (exact-integer? size) (<= 1 size 8) (make-bytevector size)) 0)]
      [(num size signed? big-endian? bstr)
       (integer->integer-bytes num size signed? big-endian? bstr 0)]))


  (define integer-bytes->integer
    (case-lambda
      [(bstr signed? big-endian? start end)
       (ensure bytevector? bstr)
       (ensure natural? start)
       (ensure natural? end)
       (case (- end start)
         [(1)
          (if signed?
              (bytevector-s8-ref bstr start)
              (bytevector-u8-ref bstr start))]
         [(2)
          (if signed?
              (bytevector-s16-ref bstr start (if big-endian?
                                                 (endianness big)
                                                 (endianness little)))
              (bytevector-u16-ref bstr start (if big-endian?
                                                 (endianness big)
                                                 (endianness little))))]
         [(4)
          (if signed?
              (bytevector-s32-ref bstr start (if big-endian?
                                                 (endianness big)
                                                 (endianness little)))
              (bytevector-u32-ref bstr start (if big-endian?
                                                 (endianness big)
                                                 (endianness little))))]
         [(8)
          (if signed?
              (bytevector-s64-ref bstr start (if big-endian?
                                                 (endianness big)
                                                 (endianness little)))
              (bytevector-u64-ref bstr start (if big-endian?
                                                 (endianness big)
                                                 (endianness little))))]
         [else
          (error 'integer-bytes->integer "length is not 1, 2, 4, or 8 bytes"
                 `(- ,end ,start)
                 (- end start))])]
      [(bstr signed?)
       (integer-bytes->integer bstr signed? (system-big-endian?) 0 (and (bytes? bstr) (bytes-length bstr)))]
      [(bstr signed? big-endian?)
       (integer-bytes->integer bstr signed? big-endian? 0 (and (bytes? bstr) (bytes-length bstr)))]
      [(bstr signed? big-endian? start)
       (integer-bytes->integer bstr signed? big-endian? start (and (bytes? bstr) (bytes-length bstr)))]))

  (define (bitwise-bit-field n start end)
    (bitwise-and (sub1 (arithmetic-shift 1 (- end start)))
                 (arithmetic-shift n (- start))))

  (define (fp->fx flonum)
    (inexact->exact (truncate flonum)))

  (define (sgn x)
    (unless (real? x) (error 'sgn "not a real number:" x))
    (cond [(= 0 x) x]  ; preserve 0, 0.0 and 0.0f0
          [(flonum? x) (cond [(fp> x 0.0) 1.0]
                             [(fp< x 0.0) -1.0]
                             [else        +nan.0])]
          [else               (if (> x 0) 1 -1)]))

  (define (bitwise-first-bit-set b)
    (if (zero? b)
        -1
        (let loop ([b b] [pos 0])
          (if (zero? (bitwise-and b 1))
              (loop (arithmetic-shift b -1) (fx+ pos 1))
              pos))))

  (define fp exact->inexact)

  (foreign-declare "#include <math.h>")

  (define pi (foreign-value "M_PI" double))

  (define epsilon.0 flonum-epsilon)
  (define (exact-floor x) (inexact->exact (floor x)))
  (define fp->exact-integer inexact->exact)
  (define fp->fx inexact->exact)

  (define random
    (case-lambda
      (()
       (pseudo-random-real))
      ((k)
       (pseudo-random-integer k)))))
