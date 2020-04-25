#lang rosette/safe

(require (prefix-in racket: racket)
         syntax/parse/define)

(define (bitvector-width v)
  ; hacky, but not sure if there's a better way
  (define bits (bitvector->natural (integer->bitvector -1 (type-of v))))
  (define n 0)
  (racket:for ([i (racket:in-naturals)])
    #:final (equal? (racket:arithmetic-shift bits (- i)) 0)
    (set! n i))
  n)
(provide bitvector-width)

(module+ test
  (require rackunit)

  (test-case "bitvector-width"
    (check-equal? (bitvector-width (bv 3 4)) 4)
    (check-equal? (bitvector-width (bv 33 8)) 8)
    (check-equal? (bitvector-width (bv 323423 64)) 64)))

(define-simple-macro (fresh-symbolic name:id type:expr)
  (let () (define-symbolic* name type) name))
(provide fresh-symbolic)
