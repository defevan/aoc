#lang racket

(define (fmt fn)
  (define lines (file->lines fn))
  (for/fold ([ht (make-immutable-hash)])
            ([l lines]
             [linen (length lines)])
    (define chars (map string->number (string-split l "")))
    (for/fold ([ht ht])
              ([c chars]
               [charn (length chars)])
      (hash-set ht (list linen charn) c))))

(define transforms
  (list (list sub1 identity) (list identity add1) (list add1 identity) (list identity sub1)))

(define (ends ht key find done)
  (append* (for/list ([t transforms])
             (define k (list ((first t) (first key)) ((second t) (second key))))
             (define v (hash-ref ht k empty))
             (cond
               [(and (equal? v find) (equal? find done)) (list k)]
               [(equal? v find) (ends ht k (add1 find) done)]
               [else empty]))))

(define (part1 ht)
  (for/sum ((key (hash-keys ht)) #:when (equal? 0 (hash-ref ht key)))
    (length (remove-duplicates (ends ht key 1 9)))))

(define (part2 ht)
  (for/sum ((key (hash-keys ht)) #:when (equal? 0 (hash-ref ht key)))
    (length (ends ht key 1 9))))

(module+ test
  (require rackunit)
  (define example (fmt "static/day10example.txt"))
  (define input (fmt "static/day10input.txt"))
  (check-equal? (part1 example) 36)
  (check-equal? (part1 input) 574)
  (check-equal? (part2 example) 81)
  (check-equal? (part2 input) 1238))

(module+ main
  (define input (fmt "static/day10input.txt"))
  (printf "day10\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
