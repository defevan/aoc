#lang racket

(define (fmt fn)
  (list->vector (for/list ([line (string-split (file->string fn) "\n")])
                  (list->vector (filter (compose not (curry string=? "")) (string-split line ""))))))

(define (exists? input pos)
  (and (> (second pos) -1)
       (> (vector-length input) (second pos))
       (> (third pos) -1)
       (> (vector-length (vector-ref input (second pos))) (third pos))))

(define (curr input)
  (for*/first ([y (vector-length input)]
               [x (vector-length (vector-ref input 0))]
               #:do ((define c (vector-ref (vector-ref input y) x)))
               #:when (ormap (curry string=? c) (list "^" ">" "v" "<")))
    (list c y x)))

(define (cont pos)
  (define rules
    (hash "^"
          (list sub1 identity)
          ">"
          (list identity add1)
          "v"
          (list add1 identity)
          "<"
          (list identity sub1)))
  (cons (car pos)
        (for/list ([f (hash-ref rules (car pos))]
                   [v (cdr pos)])
          (f v))))

(define (turn pos)
  (cons (cadr (member (first pos) (list "^" ">" "v" "<" "^"))) (cdr pos)))

(define (obstacle? input pos)
  (string=? "#" (vector-ref (vector-ref input (second pos)) (third pos))))

(define (next input pos)
  (define cont-pos (cont pos))
  (cond
    [(not (exists? input cont-pos)) '()]
    [(obstacle? input cont-pos) (turn pos)]
    [else cont-pos]))

(define (path input pos)
  (cond
    [(empty? (next input pos)) (list pos)]
    [else (cons pos (path input (next input pos)))]))

(define (part1 input)
  (length (remove-duplicates (map rest (path input (curr input))))))

(define (exits? input prior pos)
  (cond
    [(empty? pos) #t]
    [(set-member? prior pos) #f]
    [else (exits? input (set-add prior pos) (next input pos))]))

(define (vector-update vec idx val)
  (vector-append (vector-take vec idx) (vector val) (vector-drop vec (add1 idx))))

(define (input-update input pos)
  (define line (vector-update (vector-ref input (second pos)) (third pos) (first pos)))
  (vector-update input (second pos) line))

(define (part2 input)
  (define start (curr input))
  (for/sum ((pos (remove-duplicates (map rest (path input start))))
            #:do ((define next-input (input-update input (cons "#" pos))))
            #:when (not (exits? next-input (set) start)))
           1))

(module+ test
  (require rackunit)
  (define example (fmt "static/day06example.txt"))
  (define input (fmt "static/day06input.txt"))
  (check-equal? (part1 example) 41)
  (check-equal? (part1 input) 5305)
  (check-equal? (part2 example) 6)
  (check-equal? (part2 input) 2143))

(module+ main
  (define input (fmt "static/day06input.txt"))
  (printf "day06\n\tpart1: ~a\n\tpart2: ~a\n" (part1 input) (part2 input)))
