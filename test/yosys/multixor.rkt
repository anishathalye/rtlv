#lang rosette/safe

(require rackunit
         "verilog/multixor.rkt")

(test-case "xor takes multiple arguments"
  (define s0 (multixor_s #f #t #f #t #t))

  (check-equal?
   (|multixor_n result| s0)
   #t)

  (define s1 (multixor_s #f #t #f #t #f))

  (check-equal?
   (|multixor_n result| s1)
   #f))
