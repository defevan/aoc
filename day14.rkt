#lang racket

(define (fmt fn)
  (for/list ([l (file->lines fn)])
    (match-define (list px py vx vy)
      (map string->number (cdr (regexp-match #px"p=(\\d+),(\\d+)+ v=(-?\\d+),(-?\\d+)" l))))
    (list (list px py) (list vx vy))))

(define (quad? min-w max-w min-h max-h p)
  (and (>= max-w (first p) min-w) (>= max-h (last p) min-h)))

(define (part1 input w h c)
  (define result
    (map car
         (for*/fold ([robots input]) ([i c])
           (for/list ([r robots])
             (match-define (list p v) r)
             (list (list (modulo (+ (first p) (first v)) w) (modulo (+ (last p) (last v)) h)) v)))))
  (define quads
    (list (curry quad? 0 (sub1 (floor (/ w 2))) 0 (sub1 (floor (/ h 2))))
          (curry quad? (add1 (floor (/ w 2))) w 0 (sub1 (floor (/ h 2))))
          (curry quad? 0 (sub1 (floor (/ w 2))) (add1 (floor (/ h 2))) h)
          (curry quad? (add1 (floor (/ w 2))) w (add1 (floor (/ h 2))) h)))
  (for/product ((f quads)) (for/sum ((r result) #:when (f r)) 1)))

(define transforms
  (list (list add1 identity)
        (list sub1 identity)
        (list identity add1)
        (list identity sub1)
        (list add1 add1)
        (list add1 sub1)
        (list sub1 add1)
        (list sub1 sub1)))

(define (move dir pos)
  (list ((first dir) (first pos)) ((last dir) (last pos))))

(define (touching-count ht seen key)
  (hash-set! seen key #t)
  (for/sum ((t transforms) #:do ((define next (move t key)))
                           #:when (and (hash-has-key? ht next) (not (hash-has-key? seen next))))
           (add1 (touching-count ht seen next))))

(define (max-touch-count robots)
  (define ht
    (for/fold ([ht (make-immutable-hash)]) ([r robots])
      (hash-update ht (first r) add1 1)))
  (define seen (make-hash))
  (apply max
         (for/list ([r robots]
                    #:do ((define key (first r))))
           (touching-count ht seen key))))

(define (part2 input w h c)
  (match-define (list robots max-idx max)
    (for*/fold ([accum (list input -1 0)]) ([i c])
      (match-define (list robots max-idx max) accum)
      (define next-robots
        (for/list ([r robots])
          (match-define (list p v) r)
          (list (list (modulo (+ (first p) (first v)) w) (modulo (+ (last p) (last v)) h)) v)))
      (define curr-max-touch-count (max-touch-count next-robots))
      (if (> curr-max-touch-count max)
          (list next-robots i curr-max-touch-count)
          (list next-robots max-idx max))))
  (add1 max-idx))

(module+ test
  (require rackunit)
  (define example (fmt "static/day14example.txt"))
  (define input (fmt "static/day14input.txt"))
  (check-equal? (part1 example 11 7 100) 12)
  (check-equal? (part1 input 101 103 100) 229839456)
  (check-equal? (part2 input 101 103 10000) 7138))
