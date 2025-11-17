#lang racket

(define (fmt fn)
  (map (compose (curry map string->number) string-split) (file->lines fn)))

(define (is-safe? lst)
  (for/or ([f (list > <)])
    (for/and ([curr (rest lst)]
              [prev lst])
      (and (f curr prev) (<= (abs (- curr prev)) 3)))))

(define (part1 lstlst)
  (count is-safe? lstlst))

(define (combos lst)
  (stream-cons lst (sequence->stream (in-combinations lst (sub1 (length lst))))))

(define (part2 lstlst)
  (count (compose (curry stream-ormap is-safe?) combos) lstlst))

(module+ test
  (require rackunit)
  (let ([example (fmt "static/day02example.txt")]
        [input (fmt "static/day02input.txt")])
    (check-equal? (part1 example) 2)
    (check-equal? (part1 input) 390)
    (check-equal? (part2 example) 4)
    (check-equal? (part2 input) 439)))

(module+ main
  (define input (fmt "static/day02input.txt"))
  (printf "day02\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
