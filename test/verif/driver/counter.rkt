#lang verif/driver

(define (ntimes n proc)
  (if (zero? n)
      (void)
      (let ()
        (proc)
        (ntimes (sub1 n) proc))))

(define (inc n)
  ;; assert enable
  (out (input #t #t))
  ;; wait for n ticks
  (ntimes n (lambda () (tick)))
  ;; de-assert enable
  (out (input #f #t)))

(define (inc1)
  (inc 1))

(define (get)
  (let ([o (in)])
    (bitvector->natural (output-count o))))

(define (inc-all ns)
  (if (null? ns)
      (void)
      (let ()
        (inc (car ns))
        (inc-all (cdr ns)))))
