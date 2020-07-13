#lang rosette/safe

(require rosutil rackunit
         (prefix-in ! racket/base))

(define (silly)
  (define-symbolic* x y z integer?)
  (list (+ x y) z))

(define (sillyn n)
  (cond
    [(<= n 0) (silly)]
    [else (silly) (sillyn (sub1 n))]))

(test-case "no gc by default"
  (!parameterize
   ([term-cache (!make-hash)])

   (define ret (sillyn 1000))

   (!collect-garbage)

   (check-equal? (length ret) 2)
   (check-true (>= (!hash-count (term-cache)) 1000))))

(test-case "make-ephemeral-term-cache: allows gc"
  (!parameterize
   ([term-cache (!make-hash)])

   (!parameterize
    ([term-cache (make-ephemeral-term-cache)])

    (define ret (sillyn 1000))

    (!collect-garbage)

    (check-equal? (length ret) 2)
    (check-true (< (!hash-count (term-cache)) 10)))))

(test-case "term-cache-ephemeral!: allows gc"
  (!parameterize
   ([term-cache #f])

   (define base-cache (!make-hash))
   (term-cache base-cache)

   ; pre-populate with some junk, before we switch to ephemeral mode
   (sillyn 1000)

   (term-cache-ephemeral!)
   (set! base-cache #f) ; make the original hash eligible for gc

   (define ret (sillyn 10))

   (!collect-garbage)

   (check-equal? (length ret) 2)
   (check-true (< (!hash-count (term-cache)) 10))))

(test-case "make-ephemeral-term-cache: alternative to clear-terms!"
  (!parameterize
   ([term-cache (!make-hash)])

   ; based on an example that Emina gave for where (clear-terms!) is
   ; not safe to use; this is why we use the ephemeral term cache

   (term-cache-ephemeral!) ; if we skipped this, the test would fail

   (define (get-a)
     (define-symbolic a integer?)
     a)

   (define (make-junk)
     (define-symbolic* x y integer?)
     (+ x y))

   (define c (get-a))

   (make-junk)

   ; we might be tempted to call (clear-terms!) here (when we're not
   ; using the ephemeral term cache), but that would result in a bug
   ;
   ; (clear-terms!)

   (!collect-garbage)
   (check-true (<= (!hash-count (term-cache)) 1))

   (define d (get-a))

   (check-pred unsat? (verify (assert (equal? c d))))))
