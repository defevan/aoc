#lang racket

(define (mindex lst)
  (list-ref lst (floor (/ (length lst) 2))))

(define (fmt fn)
  (for/list ([part (string-split (file->string fn) "\n\n")])
    (for/list ([line (string-split part "\n")])
      (for/list ([str (string-split line #rx",|\\|")])
        (string->number str)))))

(define (valid? rules update)
  (for/and ([curr update]
            [next (cdr update)])
    (member (list curr next) rules)))

(define (reorder rules update)
  (sort update (compose (curryr member rules) list)))

(define (part1 rules updates)
  (for/sum ((update updates) #:when (valid? rules update)) (mindex update)))

(define (part2 rules updates)
  (for/sum ((update updates) #:when (not (valid? rules update))) (mindex (reorder rules update))))

(module+ test
  (require rackunit)
  (define example (fmt "static/day05example.txt"))
  (define input (fmt "static/day05input.txt"))
  (check-equal? (apply part1 example) 143)
  (check-equal? (apply part2 example) 123)
  (check-equal? (apply part1 input) 5588)
  (check-equal? (apply part2 input) 5331))

(module+ main
  (define input (fmt "static/day05input.txt"))
  (printf "day05\n\tpart1: ~a\n\tpart2: ~a\n" (apply part1 input) (apply part2 input)))
