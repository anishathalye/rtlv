#lang rosette/safe

(require rackunit
         "verilog/multixor.rkt")

(test-case "xor takes multiple arguments"
  (define s0 (multixor_s #f #t #f #t #t))

  (check-equal?
   (|multixor_n result| s0)
   #t)

  (define s1 (make-multixor_s
              [|x [0]| #t]
              [|x [1]| #f]
              [|x [2]| #t]
              [|x [3]| #f]))

  (check-equal?
   (|multixor_n result| s1)
   #f))
