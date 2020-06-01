#lang yosys

; a hand-written test case

(declare-datatype |print_test_s| ((|print_test_mk|
  (|counter_is| Bool)
  (|counter#0| Bool) ; \clk
  (|counter#1| (_ BitVec 8)) ; \count
  (|counter#4| (Array (_ BitVec 2) (_ BitVec 32))) ; \ram
)))
