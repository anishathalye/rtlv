#lang rosette/safe

(require rosutil rackunit
         (prefix-in ! (combine-in racket/base racket/set)))

(test-case "dependence: basic"
  ; solved by fast path
  (define-symbolic* x y (bitvector 32))
  (define t (bvand x (bv #x0000ffff 32)))
  (check-pred sat? (only-depends-on t (!seteq)))
  (check-pred sat? (only-depends-on t (!seteq y)))
  (check-pred unsat? (only-depends-on t (!seteq x)))
  (check-pred unsat? (only-depends-on t (!seteq x y))))

(test-case "dependence: concrete"
  (define-symbolic* x y (bitvector 32))
  (define t (bveq (bv 0 32) (concat
                             (bv -1 8)
                             (extract 11 0 x)
                             (extract 11 0 y))))
  (check-pred unsat? (only-depends-on t (!seteq)))
  (check-pred unsat? (only-depends-on t (!seteq y)))
  (check-pred unsat? (only-depends-on t (!seteq x)))
  (check-pred unsat? (only-depends-on t (!seteq x y))))

(test-case "dependence: advanced"
  ; requires solver
  (define-symbolic* x y (bitvector 32))
  (define t (if (bveq (bv 0 32) (concat
                                 (bv -1 8)
                                 (extract 11 0 x)
                                 (extract 11 0 y)))
                x
                y))
  (check-pred sat? (only-depends-on t (!seteq)))
  (check-pred unsat? (only-depends-on t (!seteq y)))
  (check-pred sat? (only-depends-on t (!seteq x)))
  (check-pred unsat? (only-depends-on t (!seteq x y))))

(test-case "dependence: unsupported"
  ; we don't support fancy things like vector right now
  (check-exn !exn:fail? (thunk (only-depends-on (vector 1 2) (!seteq)))))
