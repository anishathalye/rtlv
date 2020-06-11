#lang rosette/safe

(provide build-list replicate)

(define (build-list n proc)
  (let rec ([n (sub1 n)]
            [acc '()])
    (if (<= n 0)
        (cons (proc n) acc)
        (rec (sub1 n) (cons (proc n) acc)))))

(define (replicate n x)
  (let rec ([n n]
            [acc '()])
    (if (<= n 0)
        acc
        (rec (sub1 n) (cons x acc)))))
