#lang rosette/safe

(require rosutil
         rackunit
         (only-in racket/base values))

(test-case "build-list"
  (check-equal? (build-list 10 values) '(0 1 2 3 4 5 6 7 8 9))
  (check-equal? (build-list 5 (lambda (x) (* x x))) '(0 1 4 9 16)))

(test-case "replicate"
  (check-equal? (replicate 5 3) '(3 3 3 3 3))
  (check-equal? (replicate 3 (bv 1 8)) (list (bv 1 8) (bv 1 8) (bv 1 8))))
