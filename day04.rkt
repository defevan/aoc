#lang racket

(define (nth lst idx default)
  (if (> (length lst) idx -1)
      (list-ref lst idx)
      default))

(define (fmt fn)
  (for/list ([line (file->lines fn)])
    (filter non-empty-string? (string-split line "" #:trim? #t))))

(define (xmas-coords x y)
  (list (for/list ([mod 4])
          (list (+ x mod) (+ y 0)))
        (for/list ([mod 4])
          (list (- x 0) (- y mod)))
        (for/list ([mod 4])
          (list (+ x 0) (+ y mod)))
        (for/list ([mod 4])
          (list (- x mod) (- y 0)))
        (for/list ([mod 4])
          (list (+ x mod) (+ y mod)))
        (for/list ([mod 4])
          (list (- x mod) (- y mod)))
        (for/list ([mod 4])
          (list (+ x mod) (- y mod)))
        (for/list ([mod 4])
          (list (- x mod) (+ y mod)))))

(define (mas-x-coords x y)
  (list (list (- x 1) (- y 1))
        (list (+ x 0) (+ y 0))
        (list (+ x 1) (+ y 1))
        (list (+ x 1) (- y 1))
        (list (- x 1) (+ y 1))))

(define (is-xmas? lstlst coords)
  (string=? (apply string-append
                   (for/list ([c coords])
                     (nth (nth lstlst (first c) (list)) (last c) ".")))
            "XMAS"))

(define (is-mas-x? lstlst coords)
  (define str
    (apply string-append
           (for/list ([c coords])
             (nth (nth lstlst (first c) (list)) (last c) "."))))
  (ormap (curry string=? str) (list "MASMS" "MASSM" "SAMMS" "SAMSM")))

(define (get-coords lstlst coords-func)
  (for/list ([y (length lstlst)]
             [lst lstlst])
    (for/list ([x (length lst)])
      (coords-func x y))))

(define (part1 lstlst)
  (count (curry is-xmas? lstlst) (apply append (apply append (get-coords lstlst xmas-coords)))))

(define (part2 lstlst)
  (count (curry is-mas-x? lstlst) (apply append (get-coords lstlst mas-x-coords))))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day04example.txt")) 18)
  (check-equal? (part1 (fmt "static/day04input.txt")) 2560)
  (check-equal? (part2 (fmt "static/day04example.txt")) 9)
  (check-equal? (part2 (fmt "static/day04input.txt")) 1910))

(module+ main
  (define input (fmt "static/day04input.txt"))
  (printf "day04\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
