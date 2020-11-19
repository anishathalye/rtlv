#lang rosette/safe

(require rosutil rackunit yosys
         (prefix-in ! racket/base))

(test-case "concretize: concrete"
  (check-equal? (concretize (bv 1337 32)) (bv 1337 32)))

(test-case "concretize: basic"
  (define-symbolic* x (bitvector 8))
  (define term
    (bveq (bv -1 32) (concat (bv 0 24) x)))
  (check-false (empty? (symbolics term)))
  (check-not-eq? term #f)
  (check-eq? (concretize term) #f))

(test-case "concretize: larger"
  (define-symbolic* x (bitvector 8))
  (define term
    (bveq
     (let ([y (concat (extract 4 4 x) (bv 0 1) (extract 1 0 (concat (bv 0 2) (extract 3 3 x))))])
       (extract 3 2 (bvor y (bvshl y (bv 2 4)))))
     (extract 4 3 x)))
  (check-not-eq? term #t)
  (check-eq? (concretize term) #t))

(test-case "concretize: failure"
  (define-symbolic* x (bitvector 8))
  (define term
    (concat (bv 0 6) (extract 1 0 x)))
  (check-false (empty? (symbolics term)))
  (check-equal? term (concretize term))
  (check-exn !exn:fail? (thunk (concretize term #t))))

(test-case "concrete"
  (define-symbolic* x (bitvector 8))
  (define t1 (concat (bv 0 6) (extract 1 0 x)))
  (check-pred sat? (concrete t1))
  (define t2 (bveq (bv 0 8) (concat (bv -1 4) (extract 3 0 x))))
  (check-pred unsat? (concrete t2)))

(struct foo (bar baz) #:transparent
  #:methods gen:yosys-module
  [(define (fields _) '(bar baz))
   (define (get-field x s)
     (case s
       [(bar) (foo-bar x)]
       [(baz) (foo-baz x)]))
   (define (map-fields x f)
     (foo (f 'bar (foo-bar x))
          (f 'baz (foo-baz x))))])

(test-case "concretize-fields: subset"
  (define-symbolic* x (bitvector 8))
  (define f (foo (bveq (bv -1 32) (concat (bv 0 24) x))
                 (not (bveq (bv -1 32) (concat (bv 0 24) x)))))
  (define f*
    (!parameterize ([field-filter "bar"])
      (concretize-fields f)))
  (check-eq? (foo-bar f*) #f)
  (check-not-eq? (foo-baz f*) #t))

(test-case "concretize-fields: all"
  (define-symbolic* x (bitvector 8))
  (define f (foo (bveq (bv -1 32) (concat (bv 0 24) x))
                 (not (bveq (bv -1 32) (concat (bv 0 24) x)))))
  (define f* (concretize-fields f))
  (check-eq? (foo-bar f*) #f)
  (check-eq? (foo-baz f*) #t))

(test-case "all-values"
  (define-symbolic* x (bitvector 8))
  (define t (concat (extract 1 0 x) (bv 0 1)))
  (define all (all-values t))
  (check-equal? (!length all) 4)
  (check-not-false (!member (bv #b000 3) all))
  (check-not-false (!member (bv #b010 3) all))
  (check-not-false (!member (bv #b100 3) all))
  (check-not-false (!member (bv #b110 3) all)))

(test-case "all-values limit"
  (define-symbolic* x y (bitvector 8))
  (define t (bvxor x y))
  (check-equal? (!length (all-values t #:limit 10)) 10))
