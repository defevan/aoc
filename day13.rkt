#lang racket

(require math/matrix)

(define (fmt-btn line)
  (list (string->number (last (regexp-match #rx"X\\+([0-9]+)" line)))
        (string->number (last (regexp-match #rx"Y\\+([0-9]+)" line)))))

(define (fmt-prize line)
  (list (string->number (last (regexp-match #rx"X=([0-9]+)" line)))
        (string->number (last (regexp-match #rx"Y=([0-9]+)" line)))))

(define (fmt fn)
  (for/list ([section (string-split (file->string fn) "\r\n\r\n")])
    (define lines (string-split section "\r\n"))
    (list (fmt-btn (first lines)) (fmt-btn (second lines)) (fmt-prize (third lines)))))

(define (solve lst)
  (define A
    (matrix [[(first (first lst)) (first (second lst))]
             [(second (first lst)) (second (second lst))]]))
  (define B (matrix [[(first (third lst))] [(second (third lst))]]))
  (define result (matrix->list (matrix-solve A B)))
  (if (andmap integer? result) result null))

(define (part1 input)
  (for/sum ((region input) #:do ((define result (solve region))) #:when (not (empty? result)))
           (+ (* (first result) 3) (second result))))

(define (modify region)
  (define v 10000000000000)
  (list (first region)
        (second region)
        (list (+ (first (third region)) v) (+ (second (third region)) v))))

(define (part2 input)
  (for/sum ((region input) #:do ((define result (solve (modify region))))
                           #:when (> (length result) 0))
           (+ (* (first result) 3) (second result))))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day13example.txt")) 480)
  (check-equal? (part1 (fmt "static/day13input.txt")) 36758)
  (check-equal? (part2 (fmt "static/day13input.txt")) 76358113886726))

(module+ main
  (define input (fmt "static/day13input.txt"))
  (printf "day13\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
