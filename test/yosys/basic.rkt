#lang rosette/safe

(require rackunit
         "verilog/counter.rkt"
         "verilog/print-test.rkt"
         (only-in racket/base struct-copy string-append))

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
  (check-equal? (first outputs) (cons '|counter_n count| |counter_n count|))
  (check-equal? (length registers) 1)
  (check-equal? (first registers) (cons '|counter_n count| |counter_n count|)))

(test-case "display/write"
  (define s0 (new-zeroed-print_test_s))
  (define expected
    (apply string-append
           '("#(struct:print_test_s"
             " #f"
             " #f"
             " (bv #x00 8)"
             " #((bv #x00000000 32) (bv #x00000000 32) (bv #x00000000 32) (bv #x00000000 32))"
             ")")))
  (check-equal? (format "~a" s0) expected) ; display
  (check-equal? (format "~s" s0) expected)) ; write

(test-case "print"
  (define s0 (new-zeroed-print_test_s))
  (check-equal? (format "~v" s0) #<<EOS
print_test_s {
  clk: #f
  count: (bv #x00 8)
  ram:
    0: (bv #x00000000 32)
    1: (bv #x00000000 32)
    2: (bv #x00000000 32)
    3: (bv #x00000000 32)
}
EOS
                ))
