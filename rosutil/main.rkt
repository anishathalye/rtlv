#lang rosette/safe

(require (prefix-in racket: racket)
         syntax/parse/define)

(define-simple-macro (fresh-symbolic name:id type:expr)
  (let () (define-symbolic* name type) name))
(provide fresh-symbolic)

(define (build-list n proc)
  (let rec ([n (sub1 n)]
            [acc '()])
    (if (<= n 0)
        (cons (proc n) acc)
        (rec (sub1 n) (cons (proc n) acc)))))
(provide build-list)

(module+ test
  (require rackunit)
  (require (only-in racket values))
  (test-case "build-list"
    (check-equal? (build-list 10 values) '(0 1 2 3 4 5 6 7 8 9))
    (check-equal? (build-list 5 (lambda (x) (* x x))) '(0 1 4 9 16))))

(define (replicate n x)
  (let rec ([n n]
            [acc '()])
    (if (<= n 0)
        acc
        (rec (sub1 n) (cons x acc)))))
(provide replicate)

(module+ test
  (test-case "replicate"
    (check-equal? (replicate 5 3) '(3 3 3 3 3))
    (check-equal? (replicate 3 (bv 1 8)) (list (bv 1 8) (bv 1 8) (bv 1 8)))))

(define (concrete-head? expr)
  (not (or (term? expr) (union? expr))))
(provide concrete-head?)
