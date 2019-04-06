(import srfi-4
        chicken.foreign
        chicken.fixnum)

(define (make-bit-vector size #!optional fill)
  (let* ((fill (if fill #b11111111 0))
         (len (fx/ (fx+ size 7) 8)))
    (make-u8vector len fill)))

(define bit-vector? u8vector?)

(define (bit-vector-ref vec i)
  ((foreign-lambda* bool ((u8vector vec) (size_t i))
     "size_t byte = i / 8;"
     "size_t off = i % 8;"
     "C_return (0 != (vec[byte] & (1<<off)));")
   vec
   i))

(define (bit-vector-set! vec i x)
  ((foreign-lambda* void ((u8vector vec) (size_t i) (bool x))
     "size_t byte = i / 8;"
     "size_t off = i % 8;"
     "vec[byte] = x ? (vec[byte] | (1 << off)) : (vec[byte] & ~(1 << off));")
   vec
   i
   x))

