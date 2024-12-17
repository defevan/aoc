#lang racket

(define (fmt fn)
  (match-define (list _ ra rb rc progstr)
    (regexp-match
     #px"Register A: (\\d+).*?Register B: (\\d+).*?Register C: (\\d+).*?Program: ([0-9,]+)"
     (file->string fn)))
  (list (for/hash ([sym (list 'a 'b 'c)]
                   [str (list ra rb rc)])
          (values sym (string->number str)))
        (map string->number (string-split progstr ","))))

(define (eval registers program)
  (define out (list))
  (define (try-loop ptr)
    (if (> (length program) ptr)
        (loop ptr)
        (reverse out)))
  (define (loop ptr)
    (define lit (list-ref program (add1 ptr)))
    (define (combo)
      (match lit
        [0 0]
        [1 1]
        [2 2]
        [3 3]
        [4 (hash-ref registers 'a)]
        [5 (hash-ref registers 'b)]
        [6 (hash-ref registers 'c)]))
    (match (list-ref program ptr)
      [0
       (hash-set! registers 'a (floor (/ (hash-ref registers 'a) (expt 2 (combo)))))
       (try-loop (+ ptr 2))]
      [1
       (hash-set! registers 'b (bitwise-xor (hash-ref registers 'b) lit))
       (try-loop (+ ptr 2))]
      [2
       (hash-set! registers 'b (modulo (combo) 8))
       (try-loop (+ ptr 2))]
      [3
       (try-loop (if (zero? (hash-ref registers 'a))
                     (+ ptr 2)
                     lit))]
      [4
       (hash-set! registers 'b (bitwise-xor (hash-ref registers 'b) (hash-ref registers 'c)))
       (try-loop (+ ptr 2))]
      [5
       (set! out (cons (modulo (combo) 8) out))
       (try-loop (+ ptr 2))]
      [6
       (hash-set! registers 'b (floor (/ (hash-ref registers 'a) (expt 2 (combo)))))
       (try-loop (+ ptr 2))]
      [7
       (hash-set! registers 'c (floor (/ (hash-ref registers 'a) (expt 2 (combo)))))
       (try-loop (+ ptr 2))]))
  (try-loop 0))

(define (part1 registers program)
  (string-join (map number->string (eval (hash-copy registers) program)) ","))

(define (part2 registers program)
  (define (loop ptr curr-a)
    (for/or ([i 8]
             #:do ((define next-a (+ (* curr-a 8) i))
                   (define sub-program (eval (hash-copy (hash-set registers 'a next-a)) program)))
             #:when (equal? sub-program (drop program ptr)))
      (if (zero? ptr)
          next-a
          (loop (sub1 ptr) next-a))))
  (loop (sub1 (length program)) 0))

(module+ test
  (require rackunit)
  (check-equal? (apply part1 (fmt "static/day17example.txt")) "4,6,3,5,6,3,5,2,1,0")
  (check-equal? (apply part1 (fmt "static/day17input.txt")) "7,6,5,3,6,5,7,0,4")
  (check-equal? (apply part2 (fmt "static/day17example02.txt")) 117440)
  (check-equal? (apply part2 (fmt "static/day17input.txt")) 190615597431823))

(module+ main
  (define input (fmt "static/day17input.txt"))
  (printf "day17\n\tpart1: ~a\n\tpart2: ~a\n" (apply part1 input) (apply part2 input)))
