#lang rosette/safe

(require rosutil
         rackunit
         (only-in racket/base exn:fail?))

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
  (check-exn exn:fail? (lambda () (concretize term #t))))

(test-case "concretize-fields"
  (define-symbolic* x (bitvector 8))
  (struct foo (bar baz) #:transparent)
  (define f (foo (bveq (bv -1 32) (concat (bv 0 24) x))
                 (not (bveq (bv -1 32) (concat (bv 0 24) x)))))
  (define f* (concretize-fields foo f (bar)))
  (check-eq? (foo-bar f*) #f)
  (check-not-eq? (foo-baz f*) #t))

(test-case "concretize-all-fields"
  (define-symbolic* x (bitvector 8))
  (struct foo (bar baz) #:transparent)
  (define f (foo (bveq (bv -1 32) (concat (bv 0 24) x))
                 (not (bveq (bv -1 32) (concat (bv 0 24) x)))))
  (define f* (concretize-all-fields foo f))
  (check-eq? (foo-bar f*) #f)
  (check-eq? (foo-baz f*) #t))
