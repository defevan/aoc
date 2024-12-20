#lang racket/base

(require racket/string racket/file racket/list racket/function)

(define (fmt fn)
  (define m (string-split (file->string fn) "\r\n\r\n"))
  (list (map string->list (string-split (first m) ", ")) (map string->list (string-split (last m) "\r\n"))))

(define (take-safe lst c)
  (if (>= (length lst) c) (take lst c) lst))

(define (arrangements cache available design)
  (if (empty? design)
      1
      (for/sum ((a available) #:when (equal? a (take-safe design (length a))))
        (cached cache available (drop design (length a))))))

(define (cached cache available design)
  (if (hash-has-key? cache design)
      (hash-ref cache design)
      (let ((ret (arrangements cache available design)))
        (hash-set! cache design ret)
        ret)))

(define (part1 available designs)
  (count (compose not zero?) (map (curry cached (make-hash) available) designs)))

(define (part2 available designs)
  (apply + (map (curry cached (make-hash) available) designs)))

(module+ test
  (require rackunit)
  (check-equal? (apply part1 (fmt "static/day19example.txt")) 6)
  (check-equal? (apply part1 (fmt "static/day19input.txt")) 365)
  (check-equal? (apply part2 (fmt "static/day19example.txt")) 16)
  (check-equal? (apply part2 (fmt "static/day19input.txt")) 730121486795169))

(module+ main
  (define input (fmt "static/day19input.txt"))
  (printf "day19\n\tpart1: ~a\n\tpart2: ~a\n" (apply part1 input) (apply part2 input)))