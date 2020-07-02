#lang rosette/safe

(require rosutil rackunit
         (prefix-in ! racket/base))

(test-case "dependence: basic"
  ; solved by fast path
  (define-symbolic* x y (bitvector 32))
  (define t (bvand x (bv #x0000ffff 32)))
  (check-false (only-depends-on t '()))
  (check-false (only-depends-on t (list y)))
  (check-true (only-depends-on t (list x)))
  (check-true (only-depends-on t (list x y))))

(test-case "dependence: concrete"
  (define-symbolic* x y (bitvector 32))
  (define t (bveq (bv 0 32) (concat
                             (bv -1 8)
                             (extract 11 0 x)
                             (extract 11 0 y))))
  (check-true (only-depends-on t '()))
  (check-true (only-depends-on t (list y)))
  (check-true (only-depends-on t (list x)))
  (check-true (only-depends-on t (list x y))))

(test-case "dependence: advanced"
  ; requires solver
  (define-symbolic* x y (bitvector 32))
  (define t (if (bveq (bv 0 32) (concat
                                 (bv -1 8)
                                 (extract 11 0 x)
                                 (extract 11 0 y)))
                x
                y))
  (check-false (only-depends-on t '()))
  (check-true (only-depends-on t (list y)))
  (check-false (only-depends-on t (list x)))
  (check-true (only-depends-on t (list x y))))

(test-case "dependence: unsupported"
  ; we don't support fancy things like vector right now
  (check-exn !exn:fail? (thunk (only-depends-on (vector 1 2) '()))))
