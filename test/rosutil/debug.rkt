#lang rosette/safe

(require rosutil
         rackunit)

(test-case "value-size: non-symbolic"
  (check-equal? (value-size #t) 1)
  (check-equal? (value-size '(1 2)) 3)
  (check-equal? (value-size '(1 (2 3))) 5))

(test-case "value-size: expressions"
  (define-symbolic* x y (bitvector 32))
  (check-equal? (value-size x) 1)
  (check-equal? (value-size (bvadd x y)) 3)
  (check-equal? (value-size (bvmul x (bvadd y (bv 1337 32)))) 5))

(test-case "value-size: ite"
  (define-symbolic* b boolean?)
  (define x (if b (list 1 2) (list 2 1)))
  ; (list (ite b 1 2) (ite b 2 1))
  (check-equal? (value-size x) 9))

(test-case "value-size: symbolic unions"
  (define-symbolic* b boolean?)
  (define x (if b (list 1 2) "hi"))
  ; {[b '(1 2)] [(! b) "hi"]}
  (check-equal? (value-size x) 8))

(test-case "value-size: vectors and mutation"
  (define-symbolic* b boolean?)
  (define-symbolic* n integer?)
  (define v (vector (bv 0 5) n (bv 2 5)))
  (check-equal? (value-size v) 4)
  (vector-set! v n b)
  ; turns into a vector of unions
  ; '#({[(= 0 n) b] [(! (= 0 n)) (bv #b00000 5)]}
  ;    {[(= 1 n) b] [(! (= 1 n)) n]}
  ;    {[(= 2 n) b] [(! (= 2 n)) (bv #b00010 5)]})
  (check-equal? (value-size v) (+ 1 (* 3 (+ 1 3 1 4 1)))))
