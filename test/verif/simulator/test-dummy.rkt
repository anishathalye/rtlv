#lang rosette/safe

(require
 (prefix-in sim: "dummy-simulator.rkt")
 (only-in "../../yosys/verilog/counter.rkt" metadata)
 verif/simulator
 verif/result
 rackunit)

(test-case "basic"
  (define interpret (sim:make-interpreter '() metadata))
  (define res (interpret '(begin (init) (inc-and-get!)) (state #f #f)))
  (check-equal? (result-value res) (bv 1 8))
  (check-equal? (sim:state-dummy (state-simulator (result-state res))) 1234))
