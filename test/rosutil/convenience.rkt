#lang rosette/safe

(require rosutil
         rackunit)

(test-case "fresh-symbolic"
  (define x (fresh-symbolic "foo" (bitvector 32)))
  (define y (fresh-symbolic "foo" (bitvector 32)))
  (check-equal? (format "~a" x) "foo%0")
  (check-equal? (format "~a" y) "foo%1")
  (check-not-false (not (eq? x y))))

(test-case "fresh-memory-like"
  (define v (vector (bv 1 32) (bv 2 32)))
  (define x (fresh-memory-like "bar" v))
  (define y (fresh-memory-like "bar" v))
  (check-equal? (format "~a" x) "#(bar[0]%0 bar[1]%0)")
  (check-equal? (format "~a" y) "#(bar[0]%1 bar[1]%1)")
  (check-not-false (not (eq? x y))))

(test-case "concrete-head?"
  (define-symbolic* b boolean?)
  (check-true (concrete-head? (if b (list 1 2) (list 2 1))))
  (check-false (concrete-head? (if b (list 1 2) (list 3))))
  (check-true (concrete-head? (list 1 (if b 3 4)))))
