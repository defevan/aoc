#lang racket

(define pat #px"mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)")

(define (fmt fn)
  (for/list ([m (regexp-match* pat (file->string fn) #:match-select values)])
    (match m
      [(cons "do()" _) '(toggle #t)]
      [(cons "don't()" _) '(toggle #f)]
      [(cons _ ns) (cons 'product (map string->number ns))])))

(define (part1 input)
  (match input
    [(cons (cons 'product ns) tl) (+ (apply * ns) (part1 tl))]
    [(cons (cons 'toggle _) tl) (part1 tl)]
    [_ 0]))

(define (part2 input [run #t])
  (match input
    [(cons (cons 'product ns) tl)
     (+ (if run
            (apply * ns)
            0)
        (part2 tl run))]
    [(cons (cons 'toggle (list run)) tl) (part2 tl run)]
    [_ 0]))

(module+ test
  (require rackunit)
  (let ([example01 (fmt "static/day03example01.txt")]
        [example02 (fmt "static/day03example02.txt")]
        [input (fmt "static/day03input.txt")])
    (check-equal? (part1 example01) 161)
    (check-equal? (part1 input) 180233229)
    (check-equal? (part2 example02) 48)
    (check-equal? (part2 input) 95411583)))

(module+ main
  (define input (fmt "static/day03input.txt"))
  (printf "day03\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
