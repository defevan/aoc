#lang racket

(define (fmt fn)
  (apply map list (map (compose (curry map string->number) string-split) (file->lines fn))))

(define (part1 lstlst)
  (apply + (apply map (compose abs -) (map (curryr sort <) lstlst))))

(define (part2 lstlst)
  (apply + (map (lambda (l) (* l (count (curry = l) (last lstlst)))) (first lstlst))))

(module+ test
  (require rackunit)
  (let ([example (fmt "static/day01example.txt")]
        [input (fmt "static/day01input.txt")])
    (check-equal? (part1 example) 11)
    (check-equal? (part1 input) 1110981)
    (check-equal? (part2 example) 31)
    (check-equal? (part2 input) 24869388)))

(module+ main
  (define input (fmt "static/day01input.txt"))
  (printf "day01\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
