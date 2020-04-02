#lang rosette/safe

(require rackunit
         "verilog/counter.rkt"
         "verilog/picorv32.rkt"
         (only-in racket/base make-vector))

(test-case "zero initialization: new-zeroed-counter_s should create an instance of counter with all fields set to 0"
  (define s0 (new-zeroed-counter_s))
  (check-equal? (|counter#0| s0) #f)
  (check-equal? (|counter#1| s0) (bv 0 8))
  (check-equal? (|counter#2| s0) #f)
  (check-equal? (|counter#3| s0) #f))

(test-case "zero initialization: vectors of bitvectors should be zeroed properly"
  (define s0 (new-zeroed-picorv32_s))
  (check-equal?
    (|picorv32#168#0| s0)
    (make-vector 32 (bv 0 32))))
