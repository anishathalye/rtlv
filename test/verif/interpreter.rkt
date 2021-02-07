#lang rosette/safe

(require
 verif/interpreter
 (only-in "../yosys/verilog/counter.rkt" new-zeroed-counter_s metadata)
 rackunit)

(define (initialize0 expr)
  (make-interpreter expr (make-assoc) (new-zeroed-counter_s) metadata))

(test-case "simple builtins"
  (check-equal? (run (initialize0 '(- (+ 1 2 3 4 5) 5))) 10))

(test-case "and"
  (check-equal? (run (initialize0 '(and 1 2 3))) 3))

(test-case "lazy and"
  (check-equal? (run (initialize0 '(and #f (+ null 3)))) #f))

(test-case "or"
  (check-equal? (run (initialize0 '(or #f (list 1 2) 5 #f))) '(1 2)))

(test-case "lazy or"
  (check-equal? (run (initialize0 '(or #t (+ null 0)))) #t))

(test-case "uninterpreted"
  (check-equal? (run (initialize0 '(yield))) (void)))

(test-case "uninterpreted with args, in sequence"
  (check-equal? (run (initialize0 '(begin 3 4 (hint merge) 5))) 5))

(test-case "parallel let binding, re-binding functions"
  (define prog
    '(let ([+ (lambda (a b) (cons a b))]
           [plus +])
       (+ 3 (plus 4 5))))
  (check-equal? (run (initialize0 prog)) '(3 . 9)))

(test-case "fixpoint combinator"
  (define prog
    '(let ([Y (lambda (b) ((lambda (f) (b (lambda (x) ((f f) x))))
                           (lambda (f) (b (lambda (x) ((f f) x))))))])
       (let ([plus* (Y (lambda (rec)
                         (lambda (x)
                           (lambda (y)
                             (if (zero? x)
                                 y
                                 ((rec (sub1 x)) (add1 y)))))))])
         (let ([plus (lambda (x y) ((plus* x) y))])
           (plus 8 12)))))
  (check-equal? (run (initialize0 prog)) 20))

(test-case "recursive definitions, lets, lambdas, and apply"
  (define globals
    (let ([pairs
           (list
            (cons 'plus
                  '(lambda (x y) (if (zero? x) y (plus (sub1 x) (add1 y)))))
            (cons 'map
                  '(lambda (f l) (if (null? l) l (cons (f (car l)) (map f (cdr l)))))))])
      (map (lambda (e) (cons (car e) (closure (cdr e) (make-assoc)))) pairs)))

  (define prog
    '(let ([x (cons (bv 8 15) (cons (bv 16 15) null))]
           [y (lambda (x) (concat x (bv 1 1)))])
       (plus (bitvector->natural (apply bvadd (map y x))) 50)))

  (check-equal? (run (make-interpreter prog globals (new-zeroed-counter_s) metadata)) 100))

(test-case "varargs lambda"
  (define prog
    '(let ([first (lambda args (car args))]
           [plus (lambda args (apply + args))])
       (plus (plus 1) 2 3 (first 4 3 2 1))))
  (check-equal? (run (initialize0 prog)) 10))

(test-case "lambda compound body"
  (define prog
    '((lambda (x)
        x
        333
        (+ x 1))
      10))
  (check-equal? (run (initialize0 prog)) 11))

(test-case "let compound body"
  (define prog
    '(let ([x 3])
       (+ x 5)
       (- x 3)))
  (check-equal? (run (initialize0 prog)) 0))

(test-case "add using circuit"
  (define prog
    '(begin
       (out (input #t #t))
       (tick)
       (tick)
       (output-count (in))))
  (check-equal? (run (initialize0 prog)) (bv 2 8)))

(test-case "circuit while loop"
  (define globals
    (list
     (cons 'loop-until-wraparound
           (closure '(lambda (ctr)
                       (let ([o0 (output-count (in))])
                         (tick)
                         (if ((output-count (in)) . bvult . o0)
                             ctr
                             (loop-until-wraparound (add1 ctr)))))
                    (make-assoc)))))
  (define prog
    '(begin
       (out (input #t #t))
       (loop-until-wraparound 0)))
  (check-equal? (run (make-interpreter prog globals (new-zeroed-counter_s) metadata)) 255))

(test-case "out without args"
  (define prog
    '(begin
       (out (input #t #f))
       (equal? (out) (input #t #f))))
  (check-equal? (run (initialize0 prog)) #t))

(test-case "out*"
  (define prog
    '(begin
       (out (input* 'en #t))
       (out* 'en #f)
       (and
        (equal? (input-en (out)) #f)
        (equal? (input-nrst (out)) #f))))
  (check-equal? (run (initialize0 prog)) #t))

(test-case "lib"
  (define prog
    '(map add1 '(1 2 3 4)))
  (check-equal? (run (initialize0 prog)) '(2 3 4 5)))
