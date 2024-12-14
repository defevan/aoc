#lang racket

(define (fmt fn)
  (for/list ([l (file->lines fn)])
    (match-define (list px py vx vy)
      (map string->number (cdr (regexp-match #px"p=(\\d+),(\\d+)+ v=(-?\\d+),(-?\\d+)" l))))
    (list (list px py) (list vx vy))))

(define (in-bounds? min-w max-w min-h max-h r)
  (match-define (list (list px py) (list vx vy)) r)
  (and (>= max-w px min-w) (>= max-h py min-h)))

(define (blink w h robots)
  (for/list ([r robots])
    (match-define (list (list px py) (list vx vy)) r)
    (list (list (modulo (+ px vx) w) (modulo (+ py vy) h)) (list vx vy))))

(define (part1 input w h c)
  (define robots
    (for*/fold ([robots input]) ([i c])
      (blink w h robots)))
  (define quads
    (list (curry in-bounds? 0 (sub1 (floor (/ w 2))) 0 (sub1 (floor (/ h 2))))
          (curry in-bounds? (add1 (floor (/ w 2))) w 0 (sub1 (floor (/ h 2))))
          (curry in-bounds? 0 (sub1 (floor (/ w 2))) (add1 (floor (/ h 2))) h)
          (curry in-bounds? (add1 (floor (/ w 2))) w (add1 (floor (/ h 2))) h)))
  (for/product ((f quads)) (for/sum ((r robots) #:when (f r)) 1)))

(define (overlap? robots)
  (define ht
    (for/fold ([ht (make-immutable-hash)]) ([r robots])
      (hash-update ht (car r) add1 0)))
  (ormap (curryr > 1) (hash-values ht)))

(define (part2 input w h)
  (for/first ([i (in-naturals)]
              #:do ((set! input (blink w h input)))
              #:when (not (overlap? input)))
    (add1 i)))

(module+ test
  (require rackunit)
  (define example (fmt "static/day14example.txt"))
  (define input (fmt "static/day14input.txt"))
  (check-equal? (part1 example 11 7 100) 12)
  (check-equal? (part1 input 101 103 100) 229839456)
  (check-equal? (part2 input 101 103) 7138))

(module+ main
  (define input (fmt "static/day14input.txt"))
  (printf "day14\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input 101 103 100) (part2 input 101 103)))
