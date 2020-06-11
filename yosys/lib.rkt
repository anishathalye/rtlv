#lang rosette/safe

(require
 (for-syntax syntax/parse racket/syntax)
 syntax/parse/define
 "parameters.rkt"
 rosutil)

(provide
 = distinct _ select store bvxnor
 (rename-out [variadic-xor xor])
 ; exports from Rosette
 #%module-begin #%top-interaction #%app #%datum #%top ; module lang
 (rename-out [if ite]) true false
 (rename-out [! not] [&& and] [|| or]) ; logical
 bv ; types
 bvult bvslt bvule bvsle bvuge bvsge bvugt bvsgt ; comparison
 bvnot bvand bvor bvxor bvshl bvlshr bvashr ; bitwise
 bvneg bvadd bvsub bvmul bvudiv bvsdiv bvurem bvsrem bvsmod ; arithmetic
 concat) ; conversion

; we could implement this with `equal?`, but that is slow. Yosys uses `=` mostly for
; bitvectors, and only in a few cases for booleans. The boolean cases are:
;
; - in the invariant function, when comparing a boolean with the literal 'true' or 'false'
; - in the transition function (this is a macro anyways, that treats the '=' specially)
(define-syntax (= stx)
  (syntax-parse stx
    [(_ x:expr (~datum true))
     #'(<=> x true)]
    [(_ x:expr (~datum false))
     #'(<=> x false)]
    [(_ x:expr y:expr)
     #'(bveq x y)]))

(define (distinct x y)
  (not (bveq x y)))

(define (extractor i j)
  (lambda (x) (extract i j x)))

(define-simple-macro (_ (~datum extract) i:expr j:expr)
  (extractor i j))

(define (select a i)
  (if (array-representation-vector)
      ; vector representation
      (let ([symbolic-index (not (concrete-head? i))]
            [thresh (overapproximate-symbolic-load-threshold)])
        (if (and symbolic-index thresh (>= (vector-length a) thresh))
            ; overapproximate, return fresh symbolic value
            (fresh-symbolic overapproximated-value (type-of (vector-ref a 0)))
            ; do the indexing into the vector
            (vector-ref a (bitvector->natural i))))
      ; UF representation
      (a i)))

(define (vector-update vec pos v)
  (define symbolic-index (not (concrete-head? pos)))
  (define thresh (overapproximate-symbolic-store-threshold))
  (if (and symbolic-index thresh (>= (vector-length vec) thresh))
      (let ([type (type-of (vector-ref vec 0))])
        (list->vector
         (build-list (vector-length vec)
                     (lambda (_) (fresh-symbolic overapproximation type)))))
      ; XXX this seems inefficient
      (let ([vec-copy (apply vector (vector->list vec))])
        (vector-set! vec-copy pos v)
        vec-copy)))

(define (store a i v)
  (if (array-representation-vector)
      ; vector representation
      (vector-update a (bitvector->natural i) v)
      ; UF representation
      (lambda (i*) (if (bveq i i*) v (a i*)))))

(define (<=>* . args)
  (foldl <=> #t args))

; to match SMTLIB's xor, which can take multiple arguments
(define-syntax (variadic-xor stx)
  (syntax-parse stx
    [(_ (~seq a0 a1) ...) #'(! (<=>* (~@ a0 a1) ...))]
    [(_ a (~seq b0 b1) ...) #'(<=>* a (~@ b0 b1) ...)]))

(module+ test
  (require rackunit)
  (test-case "xor"
    (define-symbolic* a b c d e f boolean?)
    (define (reference-xor x y)
      (! (<=> x y)))
    (define (reference-variadic-xor . args)
      (foldl reference-xor #f args))
    (check-pred unsat? (verify (assert (equal? (reference-variadic-xor a) (variadic-xor a)))))
    (check-pred unsat? (verify (assert (equal? (reference-variadic-xor a b) (variadic-xor a b)))))
    (check-pred unsat? (verify (assert (equal? (reference-variadic-xor a b c) (variadic-xor a b c)))))
    (check-pred unsat? (verify (assert (equal? (reference-variadic-xor a b c d) (variadic-xor a b c d)))))
    (check-pred unsat? (verify (assert (equal? (reference-variadic-xor a b c d e) (variadic-xor a b c d e)))))))

(define (bvxnor . args)
  (bvnot (apply bvxor args)))
