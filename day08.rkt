#lang racket

(define (fmt fn)
  (for/list ([line (string-split (file->string fn) "\n")])
    (filter non-empty-string? (string-split line ""))))

(define (frequencies input)
  (filter (curry (compose not string=?) ".") (remove-duplicates (flatten input))))

(define (frequency-positions input find)
  (for*/list ([linen (length input)]
              [charn (length (first input))]
              #:when (equal? find (list-ref (list-ref input linen) charn)))
    (list linen charn)))

(define (exists? input pos)
  (and (andmap integer? pos)
       (> (first pos) -1)
       (> (length input) (first pos))
       (> (second pos) -1)
       (> (length (list-ref input (first pos))) (second pos))))

(define (antinode-positions input positions mk-antinodes)
  (append* (for*/list ([a positions]
                       [b positions]
                       #:when (not (equal? a b)))
             (filter (curry exists? input) (mk-antinodes a b)))))

(define (antinode-count input mk-antinodes)
  (length (remove-duplicates
           (append* (for/list ([f (frequencies input)])
                      (antinode-positions input (frequency-positions input f) mk-antinodes))))))

(define (part1 input)
  (antinode-count
   input
   (lambda (a b)
     (list (list (- (first b) (- (first a) (first b))) (- (second b) (- (second a) (second b))))
           (list (- (first a) (- (first b) (first a))) (- (second a) (- (second b) (second a))))))))

(define (solve-line pos1 pos2)
  (define m (/ (- (first pos2) (first pos1)) (- (last pos2) (last pos1))))
  (define b (- (first pos1) (* m (last pos1))))
  (values m b))

(define (part2 input)
  (antinode-count
   input
   (lambda (pos1 pos2)
     (define-values (m b) (solve-line pos1 pos2))
     (for/list ([linen (length input)])
       (list linen (/ (- linen b) m))))))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day08example.txt")) 14)
  (check-equal? (part1 (fmt "static/day08input.txt")) 295)
  (check-equal? (part2 (fmt "static/day08example.txt")) 34)
  (check-equal? (part2 (fmt "static/day08input.txt")) 1034))

(module+ main
  (define input (fmt "static/day08input.txt"))
  (printf "day08\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
