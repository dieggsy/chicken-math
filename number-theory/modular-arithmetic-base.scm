(import (only chicken.base error make-parameter sub1)
        (only chicken.format format))

(define current-modulus-param
  (make-parameter
   1 (lambda (n)
       (assume ((n integer))
        (cond [(<= n 0) (error 'with-modulus "not a positive integer:" n)]
             [else n])))))

(define (current-modulus)
  (current-modulus-param))

(: modular-inverse* (integer integer -> integer))
(define (modular-inverse* n a)
  (cond ((zero? a) (error 'modular-inverse "bad argument type - not a nonzero integer" a))
        ((coprime? n a) (modulo (car (bezout a n)) n))
        (else (error 'modular-inverse (format "bad argument type - not coprime to modulus ~A: ~A" n a)))))

(: modular-expt* (integer integer integer -> integer))
(define (modular-expt* n a b)
  (cond [(< b 0)  (error 'modular-expt "bad argument type - not a nonnegative integer" b)]
        [(= b 0) (if (= n 1) 0 1)]
        [else
         (let ([a (modulo a n)])
           (let loop ([b b])
             (cond [(= b 1)  a]
                   [(even? b)
                    (let ((c (loop (quotient b 2))))
                      (modulo (* c c) n))]
                   [else  (modulo (* a (loop (sub1 b))) n)])))]))

(: modular-const* (integer exact-rational -> integer))
(define (modular-const* n a)
  (cond [(integer? a)  (modulo a n)]
        [else  (modulo (* (numerator a) (modular-inverse* n (denominator a))) n)]))

(: modular-inverse (integer integer -> integer))
(define (modular-inverse a n)
  (cond ((<= n 0) (error 'modular-inverse "bad argument type - not a positive integer: ~A" n))
        (else (modular-inverse* n a))))

(: modular-expt (integer integer integer -> integer))
(define (modular-expt a b n)
  (cond ((<= n 0) (error 'modular-expt "bad orgument type - not a positive integer: ~A" n))
        (else (modular-expt* n a b))))

;; (define current-modulus-id
;;   (make-parameter #f))

(define-syntax with-modulus
  (syntax-rules ()
    [(_ modulus . body)
     (let ([n modulus])
       (parameterize ([current-modulus-param n])
         . body))]))

;; TODO: Not really sure if i need these at all...
;; (define-syntax inline-mod-op
;;   (syntax-rules ()
;;     [(_ op-macro a ...)
;;      (let ((m (current-modulus)))
;;        (op-macro m a ...))])
;;   ;; (ir-macro-transformer
;;   ;;  (lambda (exp inject compare)
;;   ;;    `(let ((m (current-modulus)))
;;   ;;       (,(inject (car exp)) ,@(cdr exp)))
;;   ;;    ;; (let ((clauses cdr exp)))
;;   ;;    ))
;;   )

;; (define-syntax fold-mod-op
;;   (syntax-rules ()
;;     [(_ op n a b)
;;      (modulo (op a b) n)]
;;     [(_ op n a b cs ...)
;;      (fold-mod-op op n (modulo (op a b) n) cs ...)]))

;; (define-syntax modular-compare
;;   (syntax-rules ()
;;     [(_ op n a) #t]
;;     [(_ op n a b) (op (modulo a n) (modulo b n))]
;;     [(_ op n a b b-expr bs ...)
;;      (let ([b (modulo b-expr n)])
;;        (and (op (modulo a n) b)
;;             (fold-mod-compare-op op n b bs ...)))]))

;; (define-syntax fold-mod-compare-op
;;   (syntax-rules ()
;;     [(_ op n a b)
;;      (op a (modulo b n))]
;;     [(_ op n a b-expr bs ...)
;;      (let ([b (modulo b-expr n)])
;;        (and (op a b)
;;             (fold-mod-compare-op op n b bs ...)))]))

;; (define-syntax modular+
;;   (syntax-rules ()
;;     [(_ n) 0]
;;     [(_ n a) (modulo a n)]
;;     [(_ n a ...) (fold-mod-op + n a ...)]))

;; (define-syntax modular*
;;   (syntax-rules ()
;;     [(_ n)  1]
;;     [(_ n a) (modulo a n)]
;;     [(_ n a ...) (fold-mod-op * n a ...)]))

;; (define-syntax modular-
;;   (syntax-rules ()
;;     [(_ n a) (modulo (- a) n)]
;;     [(_ n a b ...) (fold-mod-op - n a b ...)]))

;; (define-syntax modular/
;;   (syntax-rules ()
;;     [(_ n a) (modular-inverse a n)]
;;     [(_ n a b ...) (modular* n a (modular-inverse* n (modular* n b ...)))]))

;; (define-syntax-rule (modular-sqr n a) (modulo (* a a) n))

;; (define-syntax-rule (modular= n a b ...) (modular-compare = n a b ...))
;; (define-syntax-rule (modular< n a b ...) (modular-compare < n a b ...))
;; (define-syntax-rule (modular<= n a b ...) (modular-compare <= n a b ...))
;; (define-syntax-rule (modular> n a b ...) (modular-compare > n a b ...))
;; (define-syntax-rule (modular>= n a b ...) (modular-compare <= n a b ...))

;; (define-syntax-rule (inline-mod+ a ...) (inline-mod-op modular+ a ...))
;; (define-syntax-rule (inline-mod* a ...) (inline-mod-op modular* a ...))
;; (define-syntax-rule (inline-mod- a b ...) (inline-mod-op modular- a b ...))
;; (define-syntax-rule (inline-mod/ a b ...) (inline-mod-op modular/ a b ...))
;; (define-syntax-rule (inline-modsqr a) (inline-mod-op modular-sqr a))
;; (define (inline-modexpt a b) (inline-mod-op modular-expt* a b))
;; (define (inline-mod a) (inline-mod-op modular-const* a))
;; (define-syntax-rule (inline-mod= a b ...) (inline-mod-op modular= a b ...))
;; (define-syntax-rule (inline-mod< a b ...) (inline-mod-op modular< a b ...))
;; (define-syntax-rule (inline-mod<= a b ...) (inline-mod-op modular<= a b ...))
;; (define-syntax-rule (inline-mod> a b ...) (inline-mod-op modular> a b ...))
;; (define-syntax-rule (inline-mod>= a b ...) (inline-mod-op modular>= a b ...))
