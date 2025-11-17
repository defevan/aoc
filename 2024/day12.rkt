#lang racket

(define (fmt fn)
  (define ht (make-hash))
  (define lines (file->lines fn))
  (for/list ([l lines]
             [linen (length lines)])
    (define chars (filter non-empty-string? (string-split l "")))
    (for/list ([c chars]
               [charn (length chars)])
      (hash-set! ht (list linen charn) c)))
  ht)

(define up (list sub1 identity))
(define right (list identity add1))
(define down (list add1 identity))
(define left (list identity sub1))

(define (go dir pos)
  (list ((first dir) (first pos)) ((last dir) (last pos))))

(define (region input r key value)
  (hash-set! r key value)
  (for/list ([t (list up right down left)]
             #:do ((define next-key (go t key)))
             #:when (hash-has-key? input next-key))
    (define next-value (hash-ref input next-key))
    (cond
      [(hash-has-key? r next-key) null]
      [(equal? value next-value) (region input r next-key next-value)]))
  r)

(define (regions input)
  (for/fold ([lst '()])
            ([key (hash-keys input)]
             #:when (not (ormap (curryr hash-has-key? key) lst)))
    (cons (region input (make-hash) key (hash-ref input key)) lst)))

(define (hash-ref-safe ht key)
  (if (hash-has-key? ht key)
      (hash-ref ht key)
      null))

(define (perimeter input r)
  (for*/sum ((key (hash-keys r)) (t (list up right down left)))
            (if (equal? (hash-ref r key) (hash-ref-safe input (go t key))) 0 1)))

(define (part1 input)
  (for/sum ((r (regions input))) (* (hash-count r) (perimeter input r))))

(define (corner? input h v key value)
  (and (equal? (hash-ref-safe input (go h key)) value)
       (equal? (hash-ref-safe input (go v key)) value)
       (not (equal? (hash-ref-safe input (go h (go v key))) value))))

(define (side? input h v key value)
  (and (not (equal? (hash-ref-safe input (go h key)) value))
       (not (equal? (hash-ref-safe input (go v key)) value))))

(define (turns input region value)
  (for/sum ((key (hash-keys region)))
           (count identity
                  (for*/list ([h (list left right)]
                              [v (list up down)]
                              [f (list corner? side?)])
                    (f input h v key value)))))

(define (part2 input)
  (for/sum ((r (regions input))) (* (hash-count r) (turns input r (first (hash-values r))))))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day12example.txt")) 140)
  (check-equal? (part1 (fmt "static/day12input.txt")) 1456082)
  (check-equal? (part2 (fmt "static/day12example.txt")) 80)
  (check-equal? (part2 (fmt "static/day12input.txt")) 872382))

(module+ main
  (define input (fmt "static/day12input.txt"))
  (printf "day12\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
