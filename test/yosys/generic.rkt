#lang racket/base

(require rackunit
         yosys/generic)

(struct person (name age) #:transparent
  #:methods gen:yosys-module
  [(define (fields p) '(name age))
   (define (get-field p s)
     (case s
       [(name) (person-name p)]
       [(age) (person-age p)]))
   (define (map-fields p f)
     (person (f 'name (person-name p))
             (f 'age (person-age p))))])

(test-case "fields"
  (check-equal? (fields (person "Alice" 23)) '(name age)))

(test-case "get-field"
  (define p (person "Bob" 33))
  (check-equal? (get-field p 'name) "Bob")
  (check-equal? (get-field p 'age) 33))

(test-case "for/struct"
  (define p (person "Charlie" 24))
  (define p*
    (for/struct [(n f) p]
      (case n
        [(name) f]
        [(age) (add1 f)])))
  (check-equal? p* (person "Charlie" 25)))

(test-case "update-field"
  (define p (person "Dan" 11))
  (check-equal?
   (update-field p 'name "Daniel")
   (person "Daniel" 11)))
