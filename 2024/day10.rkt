#lang typed/racket

(define-type Key (List Number Number))

(define-type Input (HashTable Key Number))

(define (fmt (fn : String)): Input
  (define lines (file->lines fn))
  (for/fold ([ht : Input (make-immutable-hash)])
            ([l lines]
             [linen (length lines)])
    (define chars (filter integer? (map string->number (string-split l ""))))
    (for/fold : Input ([ht ht])
      ([c chars]
       [charn (length chars)])
      (hash-set ht (list linen charn) c))))

(define transforms
  (list (list sub1 identity) (list identity add1) (list add1 identity) (list identity sub1)))

(define (ends (ht : Input) (key : Key) (find : Number) (done : Number)) : (Listof Key)
  (append* (for/list : (Listof (Listof Key)) ([t transforms])
             (define k (list ((first t) (first key)) ((second t) (second key))))
             (define v (if (hash-has-key? ht k) (hash-ref ht k) empty))
             (cond
               [(and (equal? v find) (equal? find done)) (list k)]
               [(equal? v find) (ends ht k (add1 find) done)]
               [else empty]))))

(define (part1 (ht : Input)) : Number
  (for/sum ((key : Key (hash-keys ht)) #:when (equal? 0 (hash-ref ht key)))
    (length (remove-duplicates (ends ht key 1 9)))))

(define (part2 (ht : Input)) : Number
  (for/sum ((key : Key (hash-keys ht)) #:when (equal? 0 (hash-ref ht key)))
    (length (ends ht key 1 9))))

(module+ test
  (require typed/rackunit)
  (define example (fmt "static/day10example.txt"))
  (define input (fmt "static/day10input.txt"))
  (check-equal? (part1 example) 36)
  (check-equal? (part1 input) 574)
  (check-equal? (part2 example) 81)
  (check-equal? (part2 input) 1238))

(module+ main
  (define input (fmt "static/day10input.txt"))
  (printf "day10\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
