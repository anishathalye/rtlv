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
  (check-equal? (value-size (bvmul x (bvadd y (bv 1337 32)))) 5)
  (check-equal? (value-size (box (bvmul x (bv 3 32)))) 4))

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

(test-case "value-size: large term"
  (define-symbolic* x integer?)
  (define v
    (let rec ([v x] [n 100])
      (if (zero? n) v (rec (+ v v) (sub1 n)))))
  ; this doesn't explicitly measure performance, but this would take
  ; longer than the age of the universe without memoization
  (check-equal? (value-size v) (sub1 (expt 2 101))))

(test-case "value-depth: non-symbolic"
  (check-equal? (value-depth #t) 0)
  (check-equal? (value-depth '(1 2)) 1)
  (check-equal? (value-depth '(1 (2 3))) 2))

(test-case "value-depth: expressions"
  (define-symbolic* x y (bitvector 32))
  (check-equal? (value-depth x) 0)
  (check-equal? (value-depth (bvadd x y)) 1)
  (check-equal? (value-depth (bvmul x (bvadd y (bv 1337 32)))) 2)
  (check-equal? (value-depth (box (bvmul x (bv 3 32)))) 2))

(test-case "value-depth: ite"
  (define-symbolic* b boolean?)
  (define x (if b (list 1 2) (list 2 1)))
  ; (list (ite b 1 2) (ite b 2 1))
  (check-equal? (value-depth x) 2))

(test-case "value-depth: symbolic unions"
  (define-symbolic* b boolean?)
  (define x (if b (list 1 2) "hi"))
  ; {[b '(1 2)] [(! b) "hi"]}
  (check-equal? (value-depth x) 2))

(test-case "value-depth: vectors and mutation"
  (define-symbolic* b boolean?)
  (define-symbolic* n integer?)
  (define v (vector (bv 0 5) n (bv 2 5)))
  (check-equal? (value-depth v) 1)
  (vector-set! v n b)
  ; turns into a vector of unions
  ; '#({[(= 0 n) b] [(! (= 0 n)) (bv #b00000 5)]}
  ;    {[(= 1 n) b] [(! (= 1 n)) n]}
  ;    {[(= 2 n) b] [(! (= 2 n)) (bv #b00010 5)]})
  (check-equal? (value-depth v) 4))

(test-case "value-depth: large term"
  (define-symbolic* x integer?)
  (define v
    (let rec ([v x] [n 100])
      (if (zero? n) v (rec (+ v v) (sub1 n)))))
  ; this doesn't explicitly measure performance, but this would take
  ; longer than the age of the universe without memoization
  (check-equal? (value-depth v) 100))


(test-case "find-large-terms"
  (define-symbolic* x (bitvector 8))
  (define-symbolic* y (bitvector 8))
  (define-symbolic* z (bitvector 8))
  (struct module (a b c d))

  (define s (module (bvadd x y) (bvand (bvmul x (bvnot y)) z) z (bv 0 8)))
  (define state-getters (list
    (cons 'a module-a)
    (cons 'b module-b)
    (cons 'c module-c)
    (cons 'd module-d)))

  (check-equal? (find-large-terms s state-getters) 1) ; b
  (check-equal? (find-large-terms s state-getters #:threshold 1) 2)) ; a & b
