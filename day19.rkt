#lang racket

(define (fmt fn)
  (match-define (list hd tl) (string-split (file->string fn) "\r\n\r\n"))
  (list (map string->list (string-split hd ", ")) (map string->list (string-split tl "\r\n"))))

(define (take-safe lst c)
  (if (>= (length lst) c)
      (take lst c)
      '()))

(define (arrangements cache available design)
  (if (hash-has-key? cache design)
      (hash-ref cache design)
      (let ([inner (lambda ()
                     (if (empty? design)
                         1
                         (for/sum ((a available) #:when (equal? a (take-safe design (length a))))
                                  (arrangements cache available (drop design (length a))))))])
        (define ret (inner))
        (hash-set! cache design ret)
        ret)))

(define (part1 available designs #:cache [ht (make-hash)])
  (for/sum ((d designs) #:when (not (zero? (arrangements ht available d)))) 1))

(define (part2 available designs #:cache [ht (make-hash)])
  (for/sum ((d designs)) (arrangements ht available d)))

(module+ test
  (require rackunit)
  (check-equal? (apply part1 (fmt "static/day19example.txt")) 6)
  (check-equal? (apply part1 (fmt "static/day19input.txt")) 365)
  (check-equal? (apply part2 (fmt "static/day19example.txt")) 16)
  (check-equal? (apply part2 (fmt "static/day19input.txt")) 730121486795169))

(module+ main
  (define input (fmt "static/day19input.txt"))
  (printf "day19\n\tpart1: ~a\n\tpart2: ~a\n" (apply part1 input) (apply part2 input)))
