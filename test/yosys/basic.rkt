#lang rosette/safe

(require rackunit
         "verilog/counter.rkt"
         (only-in racket/base struct-copy))

(test-case "basic verification: when enable and reset are not set, value doesn't change"
  (define s0 (new-symbolic-counter_s))
  (define s1 (counter_t s0))
  (check-pred
   unsat?
   (verify
    #:assume
    (begin
      (assert (equal? (|counter_n en| s0) #f))
      (assert (equal? (|counter_n nrst| s0) #t)))
    #:guarantee
    (assert (equal? (|counter_n count| s0) (|counter_n count| s1))))))

(test-case "basic verification: counter wraparound even when no reset"
  (define s0 (struct-copy
              counter_s
              (new-symbolic-counter_s)
              [nrst #t]))
  (define s1 (counter_t s0))
  (define model
    (verify
     #:guarantee
     (assert ((|counter_n count| s0) . bvule . (|counter_n count| s1)))))
  (check-pred sat? model)
  (check-equal?
   (evaluate (|counter_n count| s0) model)
   (bv #b11111111 8)))

(test-case "inputs/outputs/registers"
  (check-equal? (length inputs) 3)
  (check-equal? (length outputs) 1)
  (check-eq? (first outputs) |counter_n count|)
  (check-equal? (length registers) 1)
  (check-eq? (first registers) |counter_n count|))
