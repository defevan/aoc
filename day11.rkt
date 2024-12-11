#lang racket

(define (fmt fn)
  (map string->number (string-split (file->string fn) " ")))

(define (digits n)
  (if (= n 0) 0 (add1 (floor (/ (log n) (log 10))))))

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

(define/memo (stones n c)
  (define dc (digits n))
  (cond ((= c 0) 1)
        ((= n 0) (stones 1 (sub1 c)))
        ((even? dc) (for/sum ((v (split n dc))) (stones v (sub1 c))))
        (else (stones (* n 2024) (sub1 c)))))

(define (part1 lst)
  (for/sum ((n lst)) (stones n 25)))

(define (part2 lst)
  (for/sum ((n lst)) (stones n 75)))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day11input.txt")) 183484)
  (check-equal? (part2 (fmt "static/day11input.txt")) 218817038947400))

(module+ main
  (define input (fmt "static/day11input.txt"))
  (printf "day11\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))