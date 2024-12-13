#lang racket

(define (fmt fn)
  (for/list ([section (string-split (file->string fn) "\r\n\r\n")])
    (define lines (string-split section "\r\n"))
    (list (map string->number (cdr (regexp-match #px"X\\+(\\d+), Y\\+(\\d+)" (first lines))))
          (map string->number (cdr (regexp-match #px"X\\+(\\d+), Y\\+(\\d+)" (second lines))))
          (map string->number (cdr (regexp-match #px"X=(\\d+), Y=(\\d+)" (third lines)))))))

(define (solve a b c)
  (define y (/ (- (* (second c) (first a)) (* (first c) (second a))) (- (* (first a) (second b)) (* (second a) (first b)))))
  (define r (list (/ (- (first c) (* (first b) y)) (first a)) y))
  (apply values (if (andmap integer? r) r (list 0 0))))

(define (part1 input)
  (for/sum ((region input))
    (define-values (a b) (apply solve region))
    (+ (* a 3) b)))

(define (modify a b c)
  (list a b (map (curry + 10000000000000) c)))

(define (part2 input)
  (part1 (map (curry apply modify) input)))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day13example.txt")) 480)
  (check-equal? (part1 (fmt "static/day13input.txt")) 36758)
  (check-equal? (part2 (fmt "static/day13input.txt")) 76358113886726))

(module+ main
  (define input (fmt "static/day13input.txt"))
  (printf "day13\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))