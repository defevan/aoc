#lang racket

(define (fmt fn)
  (map string->number (regexp-match* #px"\\b\\d+\\b" (file->string fn))))

(define (digits n)
  (if (zero? n) 0 (add1 (floor (/ (log n) (log 10))))))

(define (split n dc)
  (define x (expt 10 (/ dc 2)))
  (define st (floor (/ n x)))
  (list st (- n (* st x))))

(define-syntax-rule (define/memo (id args ...) body ...)
  (define id
    ((lambda (ht)
       (lambda (args ...)
         (hash-ref ht (list args ...)
                   (lambda () (define ret (let () body ...))
                     (hash-set! ht (list args ...) ret)
                     ret)))) (make-hash))))

(define/memo (stones n dc c)
  (cond ((zero? c) 1)
        ((zero? n) (stones 1 1 (sub1 c)))
        ((even? dc) (for/sum ((v (split n dc))) (stones v (digits v) (sub1 c))))
        (else (stones (* n 2024) (digits (* n 2024)) (sub1 c)))))

(define (part1 lst #:count [count 25])
  (for/sum ((n lst)) (stones n (digits n) count)))

(define (part2 lst)
  (part1 lst #:count 75))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day11input.txt")) 183484)
  (check-equal? (part2 (fmt "static/day11input.txt")) 218817038947400))

(module+ main
  (define input (fmt "static/day11input.txt"))
  (printf "day11\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))