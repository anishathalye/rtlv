#lang rosette/safe

(require
 (for-syntax syntax/parse racket/syntax)
 (prefix-in ! racket/base)
 syntax/parse/define
 "parameters.rkt"
 rosutil)

(provide
 = distinct _ select store bvxnor
 (rename-out [$xor xor]
             [$if ite])
 ; exports from Rosette
 true false ; constants
 (rename-out [! not] [&& and] [|| or]) ; logical
 bv ; types
 bvult bvslt bvule bvsle bvuge bvsge bvugt bvsgt ; comparison
 bvnot bvand bvor bvxor bvshl bvlshr bvashr ; bitwise
 bvneg bvadd bvsub bvmul bvudiv bvsdiv bvurem bvsrem bvsmod ; arithmetic
 concat) ; conversion

(define-simple-macro ($if test-expr then-expr else-expr)
  (if* test-expr (thunk then-expr) (thunk else-expr)))

; this is a workaround for Rosette's `if` statement producing assertions
; when it's not necessary. `if` eventually calls `speculate` to evaluate
; the then and else expressions. before doing so, `speculate` appends to
; the path condition with `test` or `(! test)`; sometimes, this immediately
; results in an exception due to the path being infeasible, and so `if`
; adds an assertion that path condition implies that the test must be true
; or false (depending on which branch failed). this assertion,
; even though it's useless, sometimes gets added to the assertion store,
; because `(&& a b)`, which is used when augmenting the path condition,
; sometimes results in a concrete Racket value of `#f`, but `(=> a (! b))`,
; which is used when adding an assertion, does not simplify in Racket to `#t`
; even though it is provably so.
;
; this thin wrapper around Rosette's `if` does this test eagerly, looking
; at the combination of the path condition with the test, and if it can be
; determined that the other path is infeasible, it skips evaluating it
; altogether.
;
; this should be safe to use with arbitrary Rosette code (even code
; e.g. involving mutation).
(define (if* test-expr then-expr else-expr)
  (define test (! (false? test-expr)))
  (!cond
   [(!or (!eq? test #t) (!not (&& (pc) (! test))))
    (then-expr)]
   [(!or (!eq? test #f) (!not (&& (pc) test)))
    (else-expr)]
   [else
    (if test (then-expr) (else-expr))]))

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
            (let-values ([(v asserted) (with-asserts (vector-ref a (bitvector->natural i)))])
              ; when Rosette can't easily prove (via rewrite rules that make this check a
              ; concrete Racket value) that we're taking a valid index into the vector,
              ; it emits an assertion that the index is in-bounds. however, we know that the index
              ; will always be in-bounds, because the Yosys-emitted code ensures this: we are indexing
              ; into a vector of length 2^n with an index that is a (bitvector n).
              ;
              ; if we wanted to, we could easily check this here by calling `(verify asserted)`
              ; to prove that the assertion could never fail. if we didn't use `with-asserts`
              ; to capture the assertion, the assertion store would keep growing, which
              ; would not be ideal.
              v)))
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
      (let-values ([(v asserted) (with-asserts (vector-update a (bitvector->natural i) v))])
        ; for reasons similar to the `select` case above, we can capture and ignore the assertions
        ; emitted during the evaluation of the vector-set!, which are related to `pos` being a
        ; valid index into the vector
        v)
      ; UF representation
      (lambda (i*) (if (bveq i i*) v (a i*)))))

(define (<=>* . args)
  (foldl <=> #t args))

; to match SMTLIB's xor, which can take multiple arguments
(define-syntax ($xor stx)
  (syntax-parse stx
    [(_ (~seq a0 a1) ...) #'(! (<=>* (~@ a0 a1) ...))]
    [(_ a (~seq b0 b1) ...) #'(<=>* a (~@ b0 b1) ...)]))

(define (bvxnor . args)
  (bvnot (apply bvxor args)))
