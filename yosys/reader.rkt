#lang racket

(require syntax/readerr)

(define (make-yosys-readtable)
  (make-readtable (current-readtable)
                  #\b 'dispatch-macro read-bitvector))

(define (is-delimiter c)
  (cond
    [(char-whitespace? c) #t]
    [else (case c
            [(#\() #t]
            [(#\)) #t]
            [(#\[) #t]
            [(#\]) #t]
            [(#\{) #t]
            [(#\}) #t]
            [(#\") #t]
            [(#\,) #t]
            [(#\') #t]
            [(#\`) #t]
            [(#\;) #t]
            [else #f])]))

(define (read-bitvector match-ch in src init-line init-col init-pos)
  (define digits
    (let loop ([acc '()])
      (let ([c (peek-char in)])
        (cond
          [(eof-object? c) acc]
          [(is-delimiter c) acc]
          [(not (or (char=? c #\0) (char=? c #\1)))
           (let-values ([(line col pos) (port-next-location in)])
             (raise-read-error (format "bad digit `~a`" c) src line col pos 1))]
          [else
           (read-char in)
           (loop (cons c acc))]))))
  (when (empty? digits)
    (raise-read-error "no digits" src init-line init-col init-pos 2))
  (define value
    (for/fold ([acc 0])
              ([i (in-list (reverse digits))])
      (+ (* acc 2) (if (char=? i #\0) 0 1))))
  (define width (length digits))
  (datum->syntax #f `(bv ,value ,width)
                 (and
                  init-line
                  (vector src init-line init-col init-pos width))))

(module+ test
  (require rackunit)

  (test-case "basic reader"
    (check-equal?
     (yosys-read (open-input-string "#b0101"))
     '(bv 5 4))))

(define (yosys-read in)
  (parameterize ([current-readtable (make-yosys-readtable)])
    (read in)))

(define (yosys-read-syntax src in)
  (parameterize ([current-readtable (make-yosys-readtable)])
    (read-syntax src in)))

(provide (rename-out
          [yosys-read read]
          [yosys-read-syntax read-syntax]))
