#lang racket/base

(require racket/file
         racket/list
         racket/function
         racket/set
         racket/treelist
         threading)

(define (fmt fn)
  (map string->number (file->lines fn)))

(define (step1 n)
  (modulo (bitwise-xor n (* n 64)) 16777216))

(define (step2 n)
  (modulo (bitwise-xor n (floor (/ n 32))) 16777216))

(define (step3 n)
  (modulo (bitwise-xor n (* n 2048)) 16777216))

(define (next n)
  (~> n step1 step2 step3))

(define (part1 input)
  (for/sum ((n input)) (for/fold ([v n]) ([_ 2000]) (next v))))

(define (part2 input)
  (define ht (make-hash)) ;; #hash((pat . count))
  (for ([n input])
    (define prev (modulo n 10))
    (define pairlst (treelist)) ;; '((last-digit . last-digit-diff))
    (define seen (mutable-set))
    (for ([i 2000])
      (set! n (next n))
      (define curr (modulo n 10))
      (set! pairlst (treelist-append pairlst (treelist (cons curr (- curr prev)))))
      (set! prev curr))
    (for ([i (- (treelist-length pairlst) 3)])
      (define pairs (treelist-take (treelist-drop pairlst i) 4))
      (define pat (treelist-map pairs cdr))
      (define count (car (treelist-last pairs)))
      (unless (set-member? seen pat)
        (set-add! seen pat)
        (hash-update! ht pat (curry + count) 0))))
  (apply max (hash-values ht)))

(module+ test
  (require rackunit)
  (check-equal? (part1 (fmt "static/day22example.txt")) 37327623)
  (check-equal? (part1 (fmt "static/day22input.txt")) 19847565303)
  (check-equal? (part2 (fmt "static/day22example02.txt")) 23)
  (check-equal? (part2 (fmt "static/day22input.txt")) 2250))

(module+ main
  (define input (fmt "static/day22input.txt"))
  (printf "day22\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))