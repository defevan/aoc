#lang racket

(define (fmt fn)
  (for/list ([l (file->lines fn)])
    (string->number l)))

(define (step1 n)
  (modulo (bitwise-xor n (* n 64)) 16777216))

(define (step2 n)
  (modulo (bitwise-xor n (floor (/ n 32))) 16777216))

(define (step3 n)
  (modulo (bitwise-xor n (* n 2048)) 16777216))

(define (next n)
  (step3 (step2 (step1 n))))

(define (part1 input)
  (for/sum ((n input))
           (for/fold ([v n]) ([i 2000])
             (next v))))

(define (last-digit n)
  (modulo n 10))

(define (part2 input)
  (define ht (make-hash)) ;; #hash((pat . count))
  (for ([n input])
    (define prev (last-digit n))
    (define pairlst '()) ;; '(last-digit . last-digit-diff)
    (define seen (make-hash))
    (for ([i 2000])
      (set! n (next n))
      (define curr (last-digit n))
      (set! pairlst (append pairlst (list (cons curr (- curr prev)))))
      (set! prev curr))
    (for ([i (- (length pairlst) 3)])
      (define pairs (take (drop pairlst i) 4))
      (define pat (map cdr pairs))
      (define count (car (last pairs)))
      (when (not (hash-has-key? seen pat))
        (hash-set! seen pat #t)
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
